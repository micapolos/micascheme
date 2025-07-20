(import (asm-3 base) (asm-3 op) (asm-3 expression) (asm-3 sized) (syntax lookup))

(check-block #xc000
  (empty-lookup)
  (empty-block)
  (dependent (aligned 1 (sized 0 (environmental (block))))))

(check-block 3
  (empty-lookup)
  (align-block 8)
  (dependent (aligned 8 (sized 0 (environmental (block))))))

(check-block #xc000
  (empty-lookup)
  (u8-block 100)
  (dependent (aligned 1 (sized 1 (environmental (block (binary 100)))))))

(check-block #xc000
  (empty-lookup)
  (u16-block #x1234 (endianness big))
  (dependent (aligned 1 (sized 2 (environmental (block (binary #x12 #x34)))))))

(check-block #xc000
  (empty-lookup)
  (u16-block #x1234 (endianness little))
  (dependent (aligned 1 (sized 2 (environmental (block (binary #x34 #x12)))))))

(check-block #xc000
  (empty-lookup)
  (bytevector-block (bytevector 10 20 30))
  (dependent (aligned 1 (sized 3 (environmental (block (binary 10 20 30)))))))

(check-block #xc000
  (empty-lookup)
  (u8-expression-block (pure-expression 123))
  (dependent (aligned 1 (sized 1 (environmental (block (binary 123)))))))

(check-block #xc000
  (lookup-with (foo 123))
  (u8-expression-block (identifier-expression #'foo))
  (dependent (foo) (aligned 1 (sized 1 (environmental (block (binary 123)))))))

(check-block 123
  (lookup-with (foo 123))
  (u8-expression-block (org-expression))
  (dependent (aligned 1 (sized 1 (environmental (block (binary 123)))))))

(check-block #xc000
  (empty-lookup)
  (u16-expression-block (pure-expression #x1234) (endianness big))
  (dependent (aligned 1 (sized 2 (environmental (block (binary #x12 #x34)))))))

(check-block #xc000
  (empty-lookup)
  (u16-expression-block (pure-expression #x1234) (endianness little))
  (dependent (aligned 1 (sized 2 (environmental (block (binary #x34 #x12)))))))

(check-block #xc000
  (empty-lookup)
  (identifier-block #'foo)
  (dependent (aligned 1 (sized 0 (environmental (foo #xc000) (block))))))

; (check-block #xc000
;   (lookup-with (foo 110))
;   (block-append
;     (empty-block)
;     (u8-block 100)
;     (u16-block #x1234 (endianness big))
;     (u16-block (org-expression) (endianness big))
;     (identifier-block #'foo))
;   (dependent (aligned 1 (sized 0 (environmental (foo #xc000) (block))))))

; (check-op #xc000
;   (empty-lookup)
;   (u8-op (pure-expression 123))
;   (op (dependent (aligned 1 (sized 1 (stack (binary 123)))))))

; (check-op #xc000 (empty-lookup)
;   (u16-op (pure-expression #x1234) (endianness big))
;   (op (dependent (aligned 1 (sized 2 (stack (binary #x12 #x34)))))))

; (check-op #xc000 (empty-lookup)
;   (u16-op (pure-expression #x1234) (endianness little))
;   (op (dependent (aligned 1 (sized 2 (stack (binary #x34 #x12)))))))

; (check-op #xc000 (empty-lookup)
;   (u16-op (org-expression) (endianness big))
;   (op (dependent (aligned 1 (sized 2 (stack (binary #xc0 #x00)))))))

; (check-op #xc000 (empty-lookup)
;   (op-append
;     (u8-op (pure-expression 10))
;     (u8-op (pure-expression 20))
;     (u8-op (pure-expression 30)))
;   (op
;     (dependent
;       (aligned 1
;         (sized 3
;           (stack
;             (binary 10)
;             (binary 20)
;             (binary 30)))))))

; (check-op #xc000 (empty-lookup)
;   (op-append
;     (u8-op (pure-expression 10))
;     (u16-op (org-expression) (endianness big))
;     (u8-op (pure-expression 20))
;     (u16-op (org-expression) (endianness big)))
;   (op
;     (dependent
;       (aligned 1
;         (sized 6
;           (stack
;             (binary 10)
;             (binary #xc0 #x01)
;             (binary 20)
;             (binary #xc0 #x04)))))))

; (check-op #xc000 (empty-lookup)
;   (op-append
;     (identifier-op #'start)
;     (u8-op (pure-expression 10))
;     (u8-op (pure-expression 20))
;     (identifier-op #'end))
;   (op
;     (dependent
;       (aligned 1
;         (sized 2
;           (stack
;             (binary 10)
;             (binary 20)))))))
