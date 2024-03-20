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
    #vu8(
      #x7f
      #x00
      #xc9
      #x04 #x20)))
