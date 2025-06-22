(import
  (rename (micascheme) (include %include))
  (asm asm)
  (asm asm-core))

(define-asm-rules
  ((ret) (db #xc9))
  ((jp nn) (db #xff) (dw nn)))

(check
  (equal?
    (asm-bytevector)
    (bytevector)))

(check
  (equal?
    (asm-bytevector (db #x12))
    (bytevector #x12)))

(check
  (equal?
    (asm-bytevector (db #x12 #x34))
    (bytevector #x12 #x34)))

(check
  (equal?
    (asm-bytevector (dw #x1234))
    (bytevector #x34 #x12)))

(check
  (equal?
    (asm-bytevector (dw #x1234 #x5678))
    (bytevector #x34 #x12 #x78 #x56)))

(check
  (equal?
    (asm-bytevector (db #x12) (db #x34))
    (bytevector #x12 #x34)))

(check
  (equal?
    (asm-bytevector (label begin) (db begin) (db end) (label end))
    (bytevector 0 2)))

(check
  (equal?
    (asm-bytevector (org 100) (label begin) (db begin) (db end) (label end))
    (bytevector 100 102)))

(check
  (equal?
    (asm-bytevector
      (org #x20)
      (label loop)
      (jp loop)
      (ret))
    (bytevector #xff #x20 0 #Xc9)))

(check
  (equal?
    (asm-bytevector (block (db 0) (db 1)))
    (bytevector 0 1)))

(check
  (equal?
    (asm-bytevector
      (org 100)
      (db x)
      (label x)
      (local
        (db x)
        (label x))
      (db x))
    (bytevector 101 102 101)))
