(import (micalang base) (micalang mica))

(check-mica
  (let
    (number->string (native-lambda number->string a-number a-string))
    (string-length (native-lambda string-length a-string a-number))
    (string-append (native-lambda string-append a-string a-string a-string))
    (string-append "Hello " (number->string (string-length "foo"))))
  "Hello 3")

(check-mica 1 1)
(check-mica (zero? 0) #t)
(check-mica (zero? 1) #f)
(check-mica (+ 1 2) 3)
(check-mica (- 3 2) 1)
(check-mica (< 1 2) #t)
(check-mica (< 2 1) #f)

(check-mica
  (let
    (x 10)
    (y 20)
    (+ x y))
  30)

(check-mica
  ((lambda (x a-number) (y a-number) (+ x y)) 2 3) 5)

(check-mica
  (let
    (a 10)
    (b 20)
    (double (x a-number) (+ x x))
    (negate (x a-number) (- 0 x))
    (negate (+ (double a) b)))
  -40)

(check-mica (if (zero? 0) "zero" "not-zero") "zero")
(check-mica (if (zero? 1) "zero" "not-zero") "not-zero")

(check-mica
  (let
    (a-fx (native a-type (constant a-fx)))
    (fx (macro (c t)
      (%%syntax-case t ()
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native a-fx (tagged (constant a-fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x a-fx) x))
    (fx+ (native-lambda fx+ a-fx a-fx a-fx))
    (fx+1 (a a-fx) (fx+ a (fx 1)))
    (fx+1 (fx+ (fx 10) (fx 20))))
  31)

; === dependent identity

(check-mica
  (let
    (id (lambda (t a-type) (x t) x))
    (id a-number 12))
  12)

; === a 2D vector of anything

(check-mica
  (let
    (a-vec2
      (lambda (t a-type)
        (
          (native-lambda list a-symbol a-type a-type)
          'a-vec2
          t)))
    (vec2
      (lambda (element a-type)
        (native-lambda cons element element (a-vec2 element))))
    (vec2 (a-vec2 a-number) (vec2 a-number 10 20) (vec2 a-number 10 20)))
  `((10 . 20) . (10 . 20)))
