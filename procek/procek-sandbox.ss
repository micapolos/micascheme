(import (micascheme) (procek procek))

(define $zx (make-zx))
(define $code
  (vector
    (display-pc)
    (display-pc)
    (display-pc)
    (jp 0)))
(zx-run $zx $code)
