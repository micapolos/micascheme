(import
  (except (micascheme) pair)
  (leo)
  (term)
  (compiler))

(define-rule-syntax (check-leo $leo $value)
  (check (equal? (leo $leo) $value)))

; === natives

(check-leo (native (+ 1 2) number) 3)

; === primitives

(check-leo #t #t)
(check-leo 123 123)
(check-leo "foo" "foo")

; === types

(check-leo (type boolean) boolean!)
(check-leo (type number) number!)
(check-leo (type string) string!)
(check-leo (type type) type!)

; === tuples

(check-leo x #f)
(check-leo (x) #f)
(check-leo (x 10) 10)
(check-leo (point (x 10) (y 20)) (cons 10 20))
(check-leo (point (x 10) (y 20) (z 30)) (vector 10 20 30))

; === tuple-ref

(check-leo (number (x 10)) 10)
(check-leo (string (x 10)) 10)

(check-leo (x (point (x 10) (y 20))) 10)
(check-leo (y (point (x 10) (y 20))) 20)
(check-leo (z (point (x 10) (y 20))) (cons 10 20))

(check-leo (x (point (x 10) (y 20) (z 30))) 10)
(check-leo (y (point (x 10) (y 20) (z 30))) 20)
(check-leo (z (point (x 10) (y 20) (z 30))) 30)
(check-leo (w (point (x 10) (y 20) (z 30))) (vector 10 20 30))

; === select ===

(check-leo (select #t (not number) (not string)) (cons 0 #t))
(check-leo (select (not boolean) 123 (not string)) (cons 1 123))
(check-leo (select (not boolean) (not number) "foo") (cons 2 "foo"))

; === switch ===

(check-leo 
  (switch 
    (select #t (not number) (not string)) 
    "boolean" "number" string) 
  "boolean")

(check-leo 
  (switch 
    (select (not boolean) 128 (not string))
    "boolean" "number" string) 
  "number")

(check-leo 
  (switch 
    (select (not boolean) (not number) "foo") 
    "boolean" "number" string) 
  "foo")

; === use / get

(check
  (equal?
    (leo
      (use
        (native string-length (any (function (length string) number)))
        (native number->string (any (function (string number) string)))
        (native string-append (any (function (append string string) string)))
        (append (string (length "foo")) " apples")))
    "3 apples"))

; === lets ===

(check
  (equal?
    (leo
      (lets
        (native string-length (any (function (length string) number)))
        (native number->string (any (function (string number) string)))
        (native string-append (any (function (append string string) string)))
        "foo"
        (length string)
        (string number)
        (append string " apples")))
    "3 apples"))

; === fib

(check
  (equal?
    (leo
      (use
        (native < (any (function (< number number) boolean)))
        (native + (any (function (+ number number) number)))
        (native - (any (function (- number number) number)))
        (use
          (recursive 
            (any (function (fib number) number))
            (function (fib number)
              (if (< number 2) number (+ (fib (- number 2)) (fib (- number 1))))))
          (fib 10))))
    55))
