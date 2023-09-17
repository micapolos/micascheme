(import (micascheme) (reactive))

(reactive
  (define zero 0)
  (define one 0)
  (lets
    (counter (value x 0 (+ x 1)))
    (value x 0 (+ x counter))))
