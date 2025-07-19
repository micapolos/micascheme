(import (asm-3 base) (asm-3 op) (asm-3 expression) (syntax lookup))

(check
  (equal?
    (block->bytevector #xc000 (empty-lookup) (empty-block))
    (bytevector)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u8-op (pure-expression 123)))
    (bytevector 123)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u16-op (pure-expression #x1234)))
    (bytevector #x34 #x12)))

(check
  (equal?
    (op->bytevector #xc000 (empty-lookup) (u16-op (org-expression)))
    (bytevector #x00 #xc0)))

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
        (u16-op (org-expression))
        (u8-op (pure-expression 20))
        (u16-op (org-expression))))
    (bytevector 10 #x01 #xc0 20 #x04 #xc0)))
