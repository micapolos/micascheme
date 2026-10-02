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
    font-char-glyph
    font-char-glyph?
    font-string-width
    font-blit-string)
  (import
    (scheme)
    (data)
    (port)
    (lets)
    (procedure)
    (fixnum)
    (switch)
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

  (define (font-char-glyph $font $char)
    (lets
      ($index (fx-/wraparound (char->integer $char) 33))
      ($glyph-vector (font-glyph-vector $font))
      (vector-ref $glyph-vector $index)))

  (define (font-char-glyph? $font $char)
    (lets
      ($index (fx-/wraparound (char->integer $char) 33))
      ($glyph-vector (font-glyph-vector $font))
      (and
        (fx>= $index 0)
        (fx< $index (vector-length $glyph-vector))
        (vector-ref $glyph-vector $index))))

  (define (font-string-width $font $string)
    (font-substring-width $font $string 0 (string-length $string)))

  (define (font-substring-width $font $string $string-start $string-end)
    (lets
      ($space-width (font-space-width $font))
      ($glyph-spacing (font-glyph-spacing $font))
      ($first-char? #t)
      ($width 0)
      (begin
        (while (not (fx= $string-start $string-end))
          (lets
            ($char (string-ref $string $string-start))
            (begin
              (if $first-char?
                (set! $first-char? #f)
                (fx+/wraparound! $width $glyph-spacing))
              (fx+/wraparound! $width
                (case $char
                  ((#\space) $space-width)
                  (else (glyph-width (font-char-glyph $font $char)))))
              (fx+1/wraparound! $string-start))))
        $width)))

  (define
    (font-blit-string
      $font
      $string
      $string-start
      $string-length
      $skip-width
      $skip-height
      $width
      $height
      $dst
      $dst-pitch
      $color)
    (lets
      ($glyph-spacing (font-glyph-spacing $font))
      ($char-index 0)
      (while
        (and
          (not (zero? $string-length))
          (> $width 0))
        (lets
          ($glyph? (font-char-glyph? $font (string-ref $string $string-start)))
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
            (fx+1/wraparound! $string-start)
            (fx-1/wraparound! $string-length)
            (fx+/wraparound! $dst (fxsll $advance 2))
            (fx-/wraparound! $skip-width $advance)
            (fx-/wraparound! $width $advance))))))
)
