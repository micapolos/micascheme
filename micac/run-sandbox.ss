(import (micascheme) (micac run))

(micac-run
  (var u8 x)
  (set x 2)
  (print first-value x)
  (add x 3)
  (print second-value x)
  (while x
    (print loop x)
    (sub x 1))
  (print the-end (+ x 100)))
