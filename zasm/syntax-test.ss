(import (micascheme) (zasm syntax) (zasm keywords))

(define-rule-syntax (check-zasm zasm expanded)
  (check
    (equal?
      (syntax->datum ((zasm-transform (lambda _ #f) #'zasm) #'$context))
      'expanded)))

(check-zasm
  (org 123)
  (context-org-set! $context 123))

(check-zasm
  (db 10)
  (context-db! $context 10))

(check-zasm
  (dw 1234)
  (context-dw! $context 1234))
