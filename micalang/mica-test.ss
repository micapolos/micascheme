(import (micalang base) (micalang mica))

(check-mica
  (let
    (number->string (native-lambda number->string any-number any-string))
    (string-length (native-lambda string-length any-string any-number))
    (string-append (native-lambda string-append any-string any-string any-string))
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
  ((lambda (x any-number) (y any-number) (+ x y)) 2 3) 5)

(check-mica
  (let
    (a 10)
    (b 20)
    (double (x any-number) (+ x x))
    (negate (x any-number) (- 0 x))
    (negate (+ (double a) b)))
  -40)

(check-mica (if (zero? 0) "zero" "not-zero") "zero")
(check-mica (if (zero? 1) "zero" "not-zero") "not-zero")

(check-mica
  (let
    (any-fx (native any-type (constant any-fx)))
    (fx (macro (c t)
      (%%syntax-case t ()
        ((_ n)
          (%%if (%%fixnum? (%%datum n))
            `(native any-fx (tagged (constant any-fx) ,(%%datum n)))
            (%%syntax-error #'n "not fixnum"))))))
    (fx-10 (fx 10))
    (fx-id (lambda (x any-fx) x))
    (fx+ (native-lambda fx+ any-fx any-fx any-fx))
    (fx+1 (a any-fx) (fx+ a (fx 1)))
    (fx+1 (fx+ (fx 10) (fx 20))))
  31)

; === dependent identity

(check-mica
  (let
    (id (lambda (t any-type) (x t) x))
    (id any-number 12))
  12)

; === a 2D vector of anything

(check-mica
  (let
    (any-vec2
      (lambda (t any-type)
        (
          (native-lambda list any-symbol any-type any-type)
          'any-vec2
          t)))
    (vec2
      (lambda (element any-type)
        (native-lambda cons element element (any-vec2 element))))
    (vec2 (any-vec2 any-number) (vec2 any-number 10 20) (vec2 any-number 10 20)))
  `((10 . 20) . (10 . 20)))
