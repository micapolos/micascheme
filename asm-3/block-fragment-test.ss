(import (asm-3 base) (asm-3 block) (asm-3 fragment) (asm-3 expression) (asm-3 block-fragment))

(check-fragment #xc000
  (empty-lookup)
  (block->fragment (empty-block))
  (dependent (aligned 1 (sized 0 (binary)))))

(check-fragment #xc000
  (lookup-with (foo 10) (bar 20))
  (block->fragment
    (block-append
      (identifier-block #'start)
      (u16-expression-block (identifier-expression #'start) (endianness big))
      (u16-expression-block (org-expression) (endianness big))
      (u8-expression-block (identifier-expression #'foo))
      (u8-expression-block (identifier-expression #'bar))
      (u8-expression-block (identifier-expression #'foo))
      (u16-expression-block (identifier-expression #'end) (endianness big))
      (identifier-block #'end)))
  (dependent (foo bar)
    (aligned 1
      (sized 9
        (binary #xc0 #x00 #xc0 #x02 10 20 10 #xc0 #x09)))))
