(import (micascheme) (asm core-2))

(define-rule-syntax (check-asm body ... bv)
  (check
    (equal?
      (asm
        body ...
        (call-with-bytevector-output-port flush))
      bv)))

(define-rules-syntaxes
  ((db n)
    (emit ($port 1) (put-u8 $port n)))
  ((dw nm)
    (emit ($port 2) (put-u16 $port nm 'little)))
  ((ret)
    (db 201))
  ((loop body ...)
    (asm
      label
      body ...
      (db #xff)
      (db label))))

(check-asm
  (db #x10)
  (bytevector #x10))

(check-asm
  (dw #x1234)
  (bytevector #x34 #x12))

(check-asm
  label-1
  (db #x10)
  label-2
  (dw #x1234)
  label-3
  (db label-1)
  (db label-2)
  (db label-3)
  (bytevector #x10 #x34 #x12 0 1 3))

(check-asm
  (db label)
  (org #x12)
  label
  (db label)
  (bytevector #x12 #x12))

(check-asm
  (ret)
  (bytevector 201))

(check-asm
  (loop
    (db 1)
    (db 2))
  (bytevector 1 2 #xff 0))

(check-asm
  (loop
    (db 1)
    (db 2))
  (loop
    (db 3)
    (db 4))
  (bytevector 1 2 #xff 0 3 4 #xff 4))
