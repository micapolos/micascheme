(import (check) (zexy asm-bytevector))

; simple asm macro
(define-asm-syntax-rule (daa)
  (db #x27))

; parameterized asm macro
(define-asm-syntax-rule (inc-dec $r)
  (begin
    (inc $r)
    (dec $r)))

(check
  (equal?
    (asm-bytevector
      (org #x2000)
      (ld a a)
      (nop)
      (daa)
      (inc-dec a)
      (ret)
      (label here)
      (dw (+ here #x10)))
    (bytevector
      #x7f      ; (ld a a)
      #x00      ; (nop)
      #x27      ; (daa)
      #x3c #x3d ; (inc a) (dec a) -> expanded from (inc-dec a)
      #xc9      ; (ret)
      #x16 #x20 ; (org #x2000) + (offset 6) + #x10
      )))

