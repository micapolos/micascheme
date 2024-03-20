(import (check) (zexy z80-bytevector))

(check
  (equal?
    (z80-bytevector
      (org #x2000)
      (ld a a)
      (nop)
      (ret)
      (label here)
      (dw (+ here #x10)))
    (bytevector
      #x7f      ; (ld a a)
      #x00      ; (nop)
      #xc9      ; (ret)
      #x13 #x20 ; (org #x2000) + (offset 3) + #x10
      )))
