(import (micascheme) (z80 asm-blob))

(check-datum=?
  (asm->syntax
    (asm 10
      (stack #`label-1 #`label-2)
      (stack #`value-1 #`value-2)
      (stack #`blob-1 #`blob-2)))
  `(lets
    label-1
    label-2
    value-1
    value-2
    (blob->bytevector (blob-append blob-1 blob-2))))
