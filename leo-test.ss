(import (micascheme) (leo))

; === natives

(check (equal? (leo (native (+ 1 2) number)) 3))

; === primitives

(check (equal? (leo #t) #t))
(check (equal? (leo 123) 123))
(check (equal? (leo "foo") "foo"))

; === types

(check (obj=? (leo (type boolean)) boolean!))
(check (obj=? (leo (type number)) number!))
(check (obj=? (leo (type string)) string!))
(check (obj=? (leo (type type)) type!))

; === tuples

(check (equal? (leo x) #f))
(check (equal? (leo (x)) #f))
(check (equal? (leo (x 10)) 10))
(check (equal? (leo (point (x 10) (y 20))) (cons 10 20)))
(check (equal? (leo (point (x 10) (y 20) (z 30))) (vector 10 20 30)))

; === tuple-ref

(check (equal? (leo (number (x 10))) 10))
(check (equal? (leo (string (x 10))) 10))

(check (equal? (leo (x (point (x 10) (y 20)))) 10))
(check (equal? (leo (y (point (x 10) (y 20)))) 20))
(check (equal? (leo (z (point (x 10) (y 20)))) (cons 10 20)))

(check (equal? (leo (x (point (x 10) (y 20) (z 30)))) 10))
(check (equal? (leo (y (point (x 10) (y 20) (z 30)))) 20))
(check (equal? (leo (z (point (x 10) (y 20) (z 30)))) 30))
(check (equal? (leo (w (point (x 10) (y 20) (z 30)))) (vector 10 20 30)))

; === use / get

(check
  (equal?
    (leo
      (use
        ((native string-length (function (length string) number))
         (native number->string (function (string number) string))
         (native string-append (function (append string string) string)))
        (append (string (length "foo")) " apples")))
    "3 apples"))
