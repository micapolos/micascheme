(import (asm-3 base) (asm-3 expression) (asm-3 dependent) (asm lookable) (asm-2 relocable) (syntax lookup))

; pure-expression
(check-expression 100
  (empty-lookup)
  (pure-expression 123)
  (expression (dependent-with () 123)))

; identifier-expression
(check
  (equal?
    (expression-ref 100
      (lookup-with (foo 123))
      (identifier-expression #'foo))
    123))

; org-expression
(check
  (equal?
    (expression-ref 100
      (lookup-with)
      (org-expression))
    100))

; application-expression
(check
  (equal?
    (expression-ref 100
      (lookup-with (ten 10))
      (application-expression
        (pure-expression +)
        (org-expression)
        (identifier-expression #'ten)
        (pure-expression 1)))
    111))

; literal
(check
  (equal?
    (expression-ref 100
      (lookup-with)
      (syntax->expression #'123))
    123))

; lookup
(check
  (equal?
    (expression-ref 100
      (lookup-with (foo 123))
      (syntax->expression #'foo))
    123))

; org
(check
  (equal?
    (expression-ref 100
      (lookup-with)
      (syntax->expression #'org))
    100))

; application
(check
  (equal?
    (expression-ref 100
      (lookup-with (+ +))
      (syntax->expression #'(+ 10 org)))
    110))
