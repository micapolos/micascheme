(import (micascheme) (term))

; === term->datum ===

(check (equal? (term->datum `foo) `foo))

(check (equal? (term->datum (abstraction 2 `foo)) `(lambda (v0 v1) foo)))
(check (equal? (term->datum (abstraction 2 (variable 0))) `(lambda (v0 v1) v1)))
(check (equal? (term->datum (abstraction 2 (variable 1))) `(lambda (v0 v1) v0)))

(check (equal? (term->datum (application `foo (list `bar `goo))) `(foo bar goo)))

; === eval-term ===

(check
  (equal?
    (eval-term
      (application `string-append (list "Hello, " "world!"))
      (environment `(micascheme)))
    "Hello, world!"))
