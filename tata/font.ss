(library (tata font)
  (export
    font
    font?
    font-height
    font-space-width
    font-glyph-spacing
    font-line-spacing
    font-glyph-vector

    read-font
    load-font
    font-glyph?
    font-blit-string)
  (import
    (scheme)
    (data)
    (port)
    (lets)
    (procedure)
    (fixnum)
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

  (define (load-font $filename)
    (call-with-port
      (open-file-input-port $filename (file-options))
      read-font))

  (define (font-glyph? $font $char)
    (lets
      ($index (fx-/wraparound (char->integer $char) 33))
      ($glyph-vector (font-glyph-vector $font))
      (and
        (fx>= $index 0)
        (fx< $index (vector-length $glyph-vector))
        (vector-ref $glyph-vector $index))))

  (define
    (font-blit-string
      $font
      $string
      $skip-width
      $skip-height
      $width
      $height
      $dst
      $dst-pitch
      $color)
    (lets
      ($string-length (string-length $string))
      ($glyph-spacing (font-glyph-spacing $font))
      ($char-index 0)
      (while
        (and
          (fx< $char-index $string-length)
          (> $width 0))
        (lets
          ($glyph? (font-glyph? $font (string-ref $string $char-index)))
          ($glyph-width
            (if $glyph?
              (glyph-width $glyph?)
              (font-space-width $font)))
          ($skip-width-max-0 (fxmax $skip-width 0))
          ($blit-width (fx-/wraparound $glyph-width $skip-width-max-0))
          ($advance (fx+/wraparound $glyph-width $glyph-spacing))
          (run
            (when
              (and $glyph? (> $blit-width 0))
              (lets
                ($glyph-bytevector (glyph-bytevector $glyph?))
                (begin
                  (blit-glyph
                    $glyph?
                    $skip-width-max-0
                    $skip-height
                    $blit-width
                    $height
                    (fx+/wraparound $dst (fxsll $skip-width-max-0 2))
                    $dst-pitch
                    $color))))
            (fx+1/wraparound! $char-index)
            (fx+/wraparound! $dst (fxsll $advance 2))
            (fx-/wraparound! $skip-width $advance)
            (fx-/wraparound! $width $advance)))))) ; fix this
)
