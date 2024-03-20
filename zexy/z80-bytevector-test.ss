(import (check) (zexy z80-bytevector))

(check
  (equal?
    (z80-bytevector
      (org #x2000)
      (ld a a)
      (nop)
      (ret)
      (label here)
      (dw (+ here 1)))
    (bytevector
      #x7f      ; (ld a a)
      #x00      ; (nop)
      #xc9      ; (ret)
      #x04 #x20 ; #x2004
      )))
