(import (check) (asm base))

(check
  (equal?
    (asm-bytevector
      (org #x2000)
      (db 10)
      (db 20)
      (label foo)
      (dw foo)
      (dw (org)))
    (bytevector 10 20)))
