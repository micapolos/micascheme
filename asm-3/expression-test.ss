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

; org-expression
(check-expression 100
  (empty-lookup)
  (offset-expression 10 (org-expression))
  (expression (dependent 110)))

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
