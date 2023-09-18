(import (micascheme) (reactive))

(reactive
  (define zero 0)
  (define counter (unit x 0 (+ x 1)))
  (define (osc d) (unit x 0 (+ x d))))
