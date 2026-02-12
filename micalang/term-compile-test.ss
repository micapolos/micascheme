(import (micalang base) (micalang term) (micalang term-compile))

(define-rule-syntax (check-compiles in out)
  (check (equal? (default-term-compile (lambda ($default $term) $term) in) 'out)))

(check-compiles
  (variable 'zero?)
  zero?)

(check-compiles
  (application (variable 'zero?) (native 1))
  (zero? 1))

(check-compiles
  (abstraction 'x (native 'int) (lambda (x) (application x x)))
  (lambda (x) (x x)))

(check-compiles
  (native '(dupa jasiu))
  (dupa jasiu))
