(library (tata glyph)
  (export
    glyph
    glyph?
    glyph-width
    glyph-bytevector

    read-glyph
    blit-glyph)
  (import
    (scheme)
    (data)
    (lets)
    (procedure)
    (port)
    (tata blit))

  (data (glyph width bytevector))

  (define (read-glyph $port)
    (lets
      ($width (get-u8-or-throw $port))
      ($bytevector (make-immobile-bytevector (fxsll $width 2)))
      ($index 0)
      (run
        (repeat $width
          (bytevector-u32-native-set! $bytevector $index (get-u32-or-throw $port))
          (set! $index (fx+/wraparound $index 4))))
      (glyph $width $bytevector)))

  (define (blit-glyph $glyph $skip-width $skip-height $width $height $dst $dst-pitch $color)
    (blit-pattern
      (fx+/wraparound
        (object->reference-address (glyph-bytevector $glyph))
        (fxsll $skip-width 2))
      $skip-height
      $width
      $height
      $dst
      $dst-pitch
      $color))
)
