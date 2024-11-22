(import (micascheme) (expression))

(define highest-priority 0)
(define not-priority 1)
(define mult-priority 2)
(define add-priority 3)
(define ?-priority 4)
(define left-to-right #t)
(define right-to-left #f)

(define (not $a) (string-append "!" $a))
(define (add $a $b) (string-append $a " + " $b))
(define (mult $a $b) (string-append $a " * " $b))
(define (? $a $b $c) (string-append $a " ? " $b " : " $c))
(define (parens $a) (string-append "(" $a ")"))

; === unary-expression

(check
  (equal?
    (unary-expression not parens
      not-priority right-to-left
      (expression highest-priority left-to-right "a"))
    (expression not-priority right-to-left "!a")))

(check
  (equal?
    (unary-expression not parens
      not-priority right-to-left
      (expression add-priority left-to-right "a + b"))
    (expression not-priority right-to-left "!(a + b)")))

(check
  (equal?
    (unary-expression not parens
      not-priority right-to-left
      (expression not-priority right-to-left "!a"))
    (expression not-priority right-to-left "!!a")))

; === binary-expression

(check
  (equal?
    (binary-expression add parens
      add-priority left-to-right
      (expression highest-priority left-to-right "a")
      (expression highest-priority left-to-right "b"))
    (expression add-priority left-to-right "a + b")))

(check
  (equal?
    (binary-expression add parens
      add-priority left-to-right
      (expression add-priority left-to-right "a + b")
      (expression add-priority left-to-right "c + d"))
    (expression add-priority left-to-right "a + b + (c + d)")))

(check
  (equal?
    (binary-expression mult parens
      mult-priority left-to-right
      (expression add-priority left-to-right "a + b")
      (expression add-priority left-to-right "c + d"))
    (expression mult-priority left-to-right "(a + b) * (c + d)")))

(check
  (equal?
    (binary-expression add parens
      add-priority left-to-right
      (expression mult-priority left-to-right "a * b")
      (expression mult-priority left-to-right "c * d"))
    (expression add-priority left-to-right "a * b + c * d")))

; === ternary-expression

(check
  (equal?
    (ternary-expression ? parens
      ?-priority right-to-left
      (expression highest-priority left-to-right "a")
      (expression highest-priority left-to-right "b")
      (expression highest-priority left-to-right "c"))
    (expression ?-priority right-to-left "a ? b : c")))

(check
  (equal?
    (ternary-expression ? parens
      ?-priority right-to-left
      (expression add-priority left-to-right "a + b")
      (expression add-priority left-to-right "c + d")
      (expression add-priority left-to-right "e + f"))
    (expression ?-priority right-to-left "a + b ? c + d : e + f")))

(check
  (equal?
    (ternary-expression ? parens
      ?-priority right-to-left
      (expression ?-priority right-to-left "a1 ? b1 : c1")
      (expression ?-priority right-to-left "a2 ? b2 : c2")
      (expression ?-priority right-to-left "a3 ? b3 : c3"))
    (expression ?-priority right-to-left "a1 ? b1 : c1 ? a2 ? b2 : c2 : (a3 ? b3 : c3)")))

