(import (micascheme) (sjasm string) (sjasm keywords))

(check
  (equal?
    (sjasm-string (nop) (ret))
    (lines-string0 "  nop" "  ret")))
