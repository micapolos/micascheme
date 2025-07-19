(import (micascheme) (asm-3 lang) (asm-3 expression) (asm-3 fragment) (asm-3 located))

(define-asm val-10 (pure-expression 10))
(define-asm db-10 (db val-10))

(check
  (equal?
    (asm 100 db-10)
    (located 100 (bytevector 10))))
