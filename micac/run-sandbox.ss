(import (micascheme) (micac run))

(micac-run
  (var u8 x)
  (add x 2)
  (print first-value x)
  (add x 3)
  (print second-value x))
