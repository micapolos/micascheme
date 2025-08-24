(import (micascheme) (zx-next emu z80))

(pretty-print
  (expand/optimize
    `(make-dispatch-table
      (make-z80
        (make-bytevector 24)
        (lambda ($addr) 0)
        (lambda ($addr $u8) (void))
        (lambda ($port) 0)
        (lambda ($port $u8) (void))))
    (environment '(micascheme) '(zx-next emu z80))))
