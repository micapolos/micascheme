(import (check) (asm asm) (micascheme))

(check
  (equal?
    (asm-bytevector
      (db #x10))
    (bytevector
      #x10)))

(check
  (equal?
    (asm-bytevector
      (db (+ #x10 #x20)))
    (bytevector
      #x30)))

(check
  (equal?
    (asm-bytevector
      (eq a #x10)
      (db a))
    (bytevector
      #x10)))

(check
  (equal?
    (asm-bytevector
      (eq a #x10)
      (eq b (+ a #x20))
      (db b))
    (bytevector
      #x30)))

(define-asm-syntax-rule (ret)
  (db #xc9))

(check
  (equal?
    (asm-bytevector
      (org #x2000)
      (db #x10)
      (dw #x1234)
      (label foo)
      (ret)
      (dw (+ foo #x10)))
    (bytevector
      #x10        ; #x10
      #x34 #x12   ; #x1234
      #xc9        ; (ret)
      #x13 #x20   ; (org #x2000) + (foo offset 3) + #x10
      )))
