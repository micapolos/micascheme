(import (asm-3 base) (asm-3 op) (asm-3 expression) (syntax lookup))

(check
  (equal?
    (block->bytevector 100 (empty-lookup) (empty-block))
    (bytevector)))

(check
  (equal?
    (block->bytevector 100
      (empty-lookup)
      (block+op (empty-block)
        (u8-op (pure-expression 123))))
    (bytevector 123)))

(check
  (equal?
    (block->bytevector 100
      (empty-lookup)
      (block+op (empty-block)
        (op-append
          (u8-op (pure-expression 10))
          (u8-op (pure-expression 20))
          (u8-op (pure-expression 30)))))
    (bytevector 10 20 30)))
