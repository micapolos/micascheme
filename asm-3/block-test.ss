(import (asm-3 base) (asm-3 block) (asm-3 expression) (syntax lookup))

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

(check-block #xc000
  (empty-lookup)
  (block-append)
  (dependent (aligned 1 (sized 0 (environmental (block))))))

; (check-block #xc000
;   (lookup-with (foo 10) (bar 20))
;   (block-append
;     (identifier-block #'start)
;     (u8-expression-block (identifier-expression #'foo))
;     (u8-expression-block (identifier-expression #'bar))
;     (u16-expression-block (org-expression) (endianness big))
;     (identifier-block #'end))
;   (dependent (foo bar)
;     (aligned 1
;       (sized 4
;         (environmental
;           (start #xc000)
;           (end #xc004)
;           (block
;             (binary 10)
;             (binary 20)
;             (binary #xc0 #x02)))))))
