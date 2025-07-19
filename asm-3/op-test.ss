(import (asm-3 base) (asm-3 op) (asm-3 expression) (syntax lookup))

(check-block #xc000
  (empty-lookup)
  (empty-block)
  (block (dependent (aligned 1 (sized 0 (stack))))))

(check-op #xc000
  (empty-lookup)
  (u8-op (pure-expression 123))
  (op (dependent (aligned 1 (sized 1 (stack (binary 123)))))))

(check-op #xc000 (empty-lookup)
  (u16-op (pure-expression #x1234) (endianness big))
  (op (dependent (aligned 1 (sized 2 (stack (binary #x12 #x34)))))))

(check-op #xc000 (empty-lookup)
  (u16-op (pure-expression #x1234) (endianness little))
  (op (dependent (aligned 1 (sized 2 (stack (binary #x34 #x12)))))))

(check-op #xc000 (empty-lookup)
  (u16-op (org-expression) (endianness big))
  (op (dependent (aligned 1 (sized 2 (stack (binary #xc0 #x00)))))))

(check-op #xc000 (empty-lookup)
  (op-append
    (u8-op (pure-expression 10))
    (u8-op (pure-expression 20))
    (u8-op (pure-expression 30)))
  (op
    (dependent
      (aligned 1
        (sized 3
          (stack
            (binary 10)
            (binary 20)
            (binary 30)))))))

(check-op #xc000 (empty-lookup)
  (op-append
    (u8-op (pure-expression 10))
    (u16-op (org-expression) (endianness big))
    (u8-op (pure-expression 20))
    (u16-op (org-expression) (endianness big)))
  (op
    (dependent
      (aligned 1
        (sized 6
          (stack
            (binary 10)
            (binary #xc0 #x01)
            (binary 20)
            (binary #xc0 #x04)))))))

(check-op #xc000 (empty-lookup)
  (op-append
    (identifier-op #'start)
    (u8-op (pure-expression 10))
    (u8-op (pure-expression 20))
    (identifier-op #'end))
  (op
    (dependent
      (aligned 1
        (sized 2
          (stack
            (binary 10)
            (binary 20)))))))
