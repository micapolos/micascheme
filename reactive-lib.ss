(library (reactive-lib)
  (export zero counter osc)
  (import (micascheme) (reactive))

  (reactive
    (define zero 0)
    (define counter (sequence n 0 (+ 1 n)))
    (define (osc dt) (sequence x 0 (fract (+ x dt))))))
