(import (check) (asm asm) (micascheme))

(check
  (equal?
    (asm-bytevector
      (org #x2000)
      (db 10)
      (db 20)
      (label foo)
      (dw foo))
    (bytevector 10 20 #x02 #x20)))
