(import (micascheme) (asm-3 expression) (asm-3 dependent) (asm lookable) (asm-2 relocable) (syntax lookup))

(define-expression foo 10)

; literal
(check
  (equal?
    (expression-ref
      (lookup-with)
      100
      (dependent-ref (syntax->dependent-expression #'123)))
    123))

; lookup
(check
  (equal?
    (expression-ref
      (lookup-with (foo 123))
      100
      (dependent-ref (syntax->dependent-expression #'foo)))
    123))

; org
(check
  (equal?
    (expression-ref
      (lookup-with)
      100
      (dependent-ref (syntax->dependent-expression #'org)))
    100))

; application
(check
  (equal?
    (expression-ref
      (lookup-with (+ +))
      100
      (dependent-ref (syntax->dependent-expression #'(+ 10 org))))
    110))

