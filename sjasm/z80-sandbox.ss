(import (micascheme) (sjasm z80))

(pretty-print
  (expand/optimize
    '(begin
      (ld a (+ 10 20))
      (lc bc de))))
