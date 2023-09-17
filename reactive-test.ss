(import (micascheme) (reactive))

(reactive
  (define zero 0)
  (define one 0)
  (value x 0 (+ x 1)))
