(import (micascheme) (sjasm expr))

(define (identifier-sjasm? $identifier)
  (free-identifier=? $identifier #'add))

(check (equal? (syntax->datum (sjasm-expr? identifier-sjasm? #'12)) 12))
(check (equal? (syntax->datum (sjasm-expr? identifier-sjasm? #'(add 1 2))) '(add 1 2)))
(check (equal? (syntax->datum (sjasm-expr? identifier-sjasm? #'(add 1 (add 2 3)))) '(add 1 (add 2 3))))
(check (equal? (sjasm-expr? identifier-sjasm? #'(+ 2 3)) #f))
(check (equal? (sjasm-expr? identifier-sjasm? #'(add 1 (- 2 3))) #f))
