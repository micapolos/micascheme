(import (micascheme) (asm-3 expression) (asm-3 dependent) (asm lookable) (asm-2 relocable) (syntax lookup))

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

