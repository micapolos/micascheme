(library (reactive-lib)
  (export zero counter osc1 osc2)
  (import (micascheme) (reactive))

  (reactive
    (define zero 0)
    (define counter (unit n 0 (+ 1 n)))
    (define osc1 (lambda (dt) (unit x 0 (+ x dt))))
    (define (osc2 dt) (unit x 0 (+ x dt)))))
