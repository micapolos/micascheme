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
    font-substring-width

    font-string-run
    font-substring-run

    font-blit-substring)
  (import
    (scheme)
    (data)
    (port)
    (lets)
    (procedure)
    (fixnum)
    (switch)
    (system)
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

  (define (font-string-run $font $string $fn)
    (font-substring-run $font $string 0 (string-length $string) $fn))

  (define (font-substring-run $font $string $string-start $string-end $fn)
    (lets
      ($space-width (font-space-width $font))
      ($glyph-spacing (font-glyph-spacing $font))
      ($first-char? #t)
      ($offset 0)
      (while (not (fx= $string-start $string-end))
        (lets
          ($char (string-ref $string $string-start))
          (begin
            (if $first-char?
              (set! $first-char? #f)
              (fx+/wraparound! $offset $glyph-spacing))
            (fx+/wraparound! $offset
              (case $char
                ((#\space) $space-width)
                (else
                  (lets
                    ($glyph (font-char-glyph $font $char))
                    ($width (glyph-width $glyph))
                    (begin
                      ($fn $glyph $offset $width)
                      $width)))))
            (fx+1/wraparound! $string-start))))))

  (define
    (font-blit-substring
      $font
      $string
      $string-start
      $string-end
      $clip-width
      $clip-height
      $width
      $height
      $dst
      $dst-pitch
      $color)
    (lets
      ($blit-dst (fx+/wraparound $dst (fx*/wraparound $clip-height $dst-pitch)))
      ($blit-height (fx-/wraparound $height $clip-height))
      (font-substring-run $font $string $string-start $string-end
        (lambda ($glyph $offset $glyph-width)
          (lets
            ($end-offset (fx+/wraparound $offset $glyph-width))
            ($skip-width (fxmax 0 (fx-/wraparound $clip-width $offset)))
            ($skip-end-width (logging (fxmax 0 (fx-/wraparound $end-offset $width))))
            (and
              (> $end-offset $clip-width)
              (< $offset $width)
              (blit-glyph
                $glyph
                $skip-width
                $clip-height
                (fx-/wraparound (fx-/wraparound $glyph-width $skip-width) $skip-end-width)
                $blit-height
                (fx+/wraparound $blit-dst (fxsll (fx+/wraparound $offset $skip-width) 2))
                $dst-pitch
                $color)))))))
)
