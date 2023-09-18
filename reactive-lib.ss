(library (reactive-lib)
  (export zero counter osc)
  (import (micascheme) (reactive))

  (reactive
    (define zero 0)
    (define counter (unit n 0 (+ 1 n)))
    (define (osc dx) (unit x 0 (+ x dx)))))
