(import (asm-3 base) (asm-3 expression) (asm-3 dependent) (asm lookable) (asm-2 relocable) (syntax lookup))

; pure-expression
(check-expression 100
  (empty-lookup)
  (pure-expression 123)
  (expression (dependent 123)))

; identifier-expression
(check-expression 100
  (lookup-with (foo 123))
  (identifier-expression #'foo)
  (expression (dependent (foo) 123)))

; org-expression
(check-expression 100
  (empty-lookup)
  (org-expression)
  (expression (dependent 100)))

; application-expression
(check-expression 100
  (lookup-with
    (val-10 10)
    (val-20 20))
  (application-expression
    (pure-expression +)
    (org-expression)
    (identifier-expression #'val-10)
    (identifier-expression #'val-20)
    (pure-expression 1))
  (expression (dependent (val-10 val-20) 131)))

; literal
(check-expression 100
  (empty-lookup)
  (syntax->expression #'123)
  (expression (dependent 123)))

; lookup
(check-expression 100
  (lookup-with (foo 123))
  (syntax->expression #'foo)
  (expression (dependent (foo) 123)))

; org
(check-expression 100
  (empty-lookup)
  (syntax->expression #'org)
  (expression (dependent 100)))

; application
(check-expression 100
  (lookup-with
    (add +)
    (val-20 20))
  (syntax->expression #'(add 10 val-20 org))
  (expression (dependent (add val-20) 130)))
