(import (micascheme) (sjasm sjasm) (sjasm z80))

(pretty-print
  (expand
    '(sjasm
      (ld a 10))
    (environment '(micascheme) '(sjasm sjasm) '(sjasm z80))))
