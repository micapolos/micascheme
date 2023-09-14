(library (animated)
  (export
    animated animated-values animated-advance-fn
    animated-advance animated-values
    triangle-wave)
  (import (micascheme))

  (data (animated value advance-fn))

  (define (animated-advance $animated $dt)
    ((animated-advance-fn $animated) $dt))

  (define (animated-values $animated $dt $count)
    (reverse
      (car
        (iterate
          (lambda ($stack-animated-pair)
            (lets
              ($stack (car $stack-animated-pair))
              ($animated (cdr $stack-animated-pair))
              (cons
                (push $stack (animated-value $animated))
                (animated-advance $animated $dt))))
          (cons (stack) $animated)
          $count))))

  ; --------------------------------

  (define (triangle-wave $phase)
    (lets
      ($phase (fract $phase))
      (animated $phase
        (lambda ($dt)
          (triangle-wave (+ $phase $dt))))))
)
