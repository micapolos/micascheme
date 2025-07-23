(import (asm-3 base) (asm-3 expression) (asm-3 syntax-expression) (syntax lookup))

; literal
(check-expression 100
  (empty-lookup)
  (syntax->expression (empty-lookup) #'123)
  (expression (dependent 123)))

; lookup
(check-expression 100
  (lookup-with (foo 123))
  (syntax->expression (empty-lookup) #'foo)
  (expression (dependent (foo) 123)))

; org
(check-expression 100
  (empty-lookup)
  (syntax->expression (empty-lookup) #'org)
  (expression (dependent 100)))

; application
(check-expression 100
  (lookup-with
    (add +)
    (val-20 20))
  (syntax->expression (empty-lookup) #'(add 10 val-20 org))
  (expression (dependent (add val-20) 130)))
