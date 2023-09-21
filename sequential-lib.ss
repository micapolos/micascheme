(library (sequential-lib)
  (export zero counter osc variable)
  (import (micascheme) (sequential))

  (sequential
    (define zero 0)

    (define counter (sequence n 0 (+ 1 n)))

    (define (osc dt)
      (sequence x 0 (fract (+ x dt))))

    (define (variable init set? value)
      (sequence x init (if set? value x)))))
