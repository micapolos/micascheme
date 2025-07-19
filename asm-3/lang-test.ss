(import (micascheme) (asm-3 lang) (asm-3 expression) (asm-3 fragment) (asm-3 assembled))

(define-asm val-10 (pure-expression 10))
(define-asm db-10 (db val-10))

(define-rule-syntax (ret) (db 201))
(define-asm empty-proc (ret))

(check
  (equal?
    (assembled-asm (org 100) db-10)
    (assembled 100 (bytevector 10))))

(check
  (equal?
    (assembled-asm (org 100) empty-proc)
    (assembled 100 (bytevector 201))))
