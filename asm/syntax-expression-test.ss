(import (asm base) (asm expression) (asm syntax-expression) (syntax lookup))

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

(check-expression
  (syntax->expression
    (lookup-with
      (plus (syntax-rules () ((_ a b) (+ a b)))))
    #'(plus 10 20))
  (dependent (+ 10 20)))

; length
(check-expression
  (syntax->expression (empty-lookup) #'(length foo bar 123 #'asd))
  (dependent 4))

