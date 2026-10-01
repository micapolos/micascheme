(library (tata blit)
  (export
    blit-pattern-line
    blit-pattern)
  (import
    (scheme)
    (syntax)
    (foreign)
    (lets)
    (procedure)
    (fixnum))

  (define-rule-syntax (blit-pattern-line src height dst dst-pitch color)
    (lets
      ($src src)
      ($height height)
      ($dst dst)
      ($dst-pitch dst-pitch)
      ($color color)
      (while (not (zero? $height))
        (when (not (zero? (fxand $src 1)))
          (foreign-set-u32! $dst $color))
        (fxsrl! $src 1)
        (fx-1/wraparound! $height)
        (fx+/wraparound! $dst $dst-pitch))))

  (define-rule-syntax (blit-pattern src src-shift width height dst dst-pitch color)
    (lets
      ($src src)
      ($src-shift src-shift)
      ($width width)
      ($height height)
      ($dst dst)
      ($dst-pitch dst-pitch)
      ($color color)
      (while (not (zero? $width))
        (blit-pattern-line
          (fxsrl (foreign-u32 $src) $src-shift)
          $height
          $dst
          $dst-pitch
          $color)
        (fx+/wraparound! $src 4)
        (fx-1/wraparound! $width)
        (fx+/wraparound! $dst 4))))
)
