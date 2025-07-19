(import (asm-3 base) (asm-3 op) (asm-3 expression) (syntax lookup))

(check-block #xc000
  (empty-lookup)
  (empty-block)
  (block (dependent (aligned 1 (sized 0 (stack))))))

(check-op #xc000
  (empty-lookup)
  (u8-op (pure-expression 123))
  (op (dependent (aligned 1 (sized 1 (stack (binary 123)))))))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u16-op (pure-expression #x1234) (endianness big)))
    (bytevector #x12 #x34)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u16-op (pure-expression #x1234) (endianness little)))
    (bytevector #x34 #x12)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u16-op (org-expression) (endianness big)))
    (bytevector #xc0 #x00)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup)
      (op-append
        (u8-op (pure-expression 10))
        (u8-op (pure-expression 20))
        (u8-op (pure-expression 30))))
    (bytevector 10 20 30)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup)
      (op-append
        (u8-op (pure-expression 10))
        (u16-op (org-expression) (endianness big))
        (u8-op (pure-expression 20))
        (u16-op (org-expression) (endianness big))))
    (bytevector 10 #xc0 #x01 20 #xc0 #x04)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup)
      (op-append
        (identifier-op #'start)
        (u8-op (pure-expression 10))
        (u8-op (pure-expression 20))
        (identifier-op #'end)))
    (bytevector 10 20)))
