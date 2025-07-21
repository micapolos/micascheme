(import (asm-3 base) (asm-3 block) (asm-3 fragment) (asm-3 expression) (asm-3 block-fragment))

(check
  (equal?
    (fragment->bytevector #xc000 (empty-lookup) (block->fragment (empty-block)))
    (bytevector)))

(check
  (equal?
    (fragment->bytevector #xc000 (empty-lookup)
      (block->fragment
        (block-append
          (identifier-block #'start)
          (u16-expression-block (identifier-expression #'start) (endianness big))
          (u16-expression-block (org-expression) (endianness big))
          (u16-expression-block (identifier-expression #'end) (endianness big))
          (identifier-block #'end))))
    (bytevector #xc0 #x00 #xc0 #x02 #xc0 #x06)))
