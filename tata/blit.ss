(library (tata blit)
  (export
    blit-pattern-line
    blit-pattern)
  (import
    (scheme)
    (syntax)
    (foreign))

  (define-rule-syntax (blit-pattern-line src height dst dst-pitch color)
    (let
      (
        ($dst dst)
        ($dst-pitch dst-pitch)
        ($color color))
      (let loop
        (
          ($src src)
          ($height height)
          ($dst dst))
        (and
          (not (zero? $height))
          (begin
            (when
              (not (zero? (fxand $src 1)))
              (foreign-set-u32! $dst $color))
            (loop
              (fxsrl $src 1)
              (fx-/wraparound $height 1)
              (fx+/wraparound $dst $dst-pitch)))))))

  (define-rule-syntax (blit-pattern src src-shift width height dst dst-pitch color)
    (let
      (
        ($src-shift src-shift)
        ($height height)
        ($dst-pitch dst-pitch)
        ($color color))
      (let loop
        (
          ($src src)
          ($width width)
          ($dst dst))
        (and
          (not (zero? $width))
          (begin
            (blit-pattern-line
              (fxsrl (foreign-u32 $src) $src-shift)
              $height
              $dst
              $dst-pitch
              $color)
            (loop
              (fx+/wraparound $src 4)
              (fx-/wraparound $width 1)
              (fx+/wraparound $dst 4)))))))
)
