(import (asm-3 base) (asm-3 expression) (asm-3 syntax-expression) (syntax lookup))

; literal
(check-expression
  (syntax->expression (empty-lookup) #'123)
  (dependent 123))

; lookup
(check-expression
  (syntax->expression (empty-lookup) #'foo)
  (dependent (foo) foo))

; application
(check-expression
  (syntax->expression (empty-lookup) #'(fn 10 val-20))
  (dependent (fn val-20) (fn 10 val-20)))

; +
(check-expression
  (syntax->expression (empty-lookup) #'(+ 10 val-10 val-20))
  (dependent (val-10 val-20) (+ 10 val-10 val-20)))

; -
(check-expression
  (syntax->expression (empty-lookup) #'(- 10 val-10 val-20))
  (dependent (val-10 val-20) (- 10 val-10 val-20)))
