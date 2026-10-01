(library (tata font)
  (export
    font
    font?
    font-height
    font-space-width
    font-glyph-spacing
    font-line-spacing
    font-glyph-vector

    read-font)
  (import
    (scheme)
    (data)
    (port)
    (lets)
    (procedure)
    (tata glyph))

  (data (font height space-width glyph-spacing line-spacing glyph-vector))

  (define (read-font $port)
    (lets
      ($height (get-u8-or-throw $port))
      ($space-width (get-u8-or-throw $port))
      ($glyph-spacing (get-u8-or-throw $port))
      ($line-spacing (get-u8-or-throw $port))
      ($glyph-count (get-u8-or-throw $port))
      ($glyph-vector (make-vector $glyph-count))
      ($index 0)
      (run
        (repeat $glyph-count
          (vector-set! $glyph-vector $index (read-glyph $port))
          (set! $index (fx+/wraparound $index 1))))
      (font $height $space-width $glyph-spacing $line-spacing (vector->immutable-vector $glyph-vector))))
)
