(import (micac unit))

(define-unit (incrementor)
  (init
    (var int input 0))
  (update
    (set+ input 1)))
