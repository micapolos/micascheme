(import (check) (zexy z80-bytevector))

(define-z80-syntax-rule (inc-dec $r)
  (begin
    (inc $r)
    (dec $r)))

(check
  (equal?
    (z80-bytevector
      (org #x2000)
      (ld a a)
      (nop)
      (inc-dec a)
      (ret)
      (label here)
      (dw (+ here #x10)))
    (bytevector
      #x7f      ; (ld a a)
      #x00      ; (nop)
      #x3c #x3d ; (inc a) (dec a) -> expanded from (inc-dec a)
      #xc9      ; (ret)
      #x15 #x20 ; (org #x2000) + (offset 5) + #x10
      )))

