(library (sequential-lib)
  (export zero counter osc variable)
  (import (micascheme) (sequential))

  (sequential
    (define zero 0)

    (define counter (sequence 0 n (+ 1 n)))

    (define (osc dt)
      (sequence 0 x (fract (+ x dt))))

    (define (variable init set? value)
      (sequence init x (if set? value x))))
)
