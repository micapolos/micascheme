(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === native

(check-compiles
  (native boolean (prim +))
  (boolean (literal (prim +))))

(check-compiles
  (native (pi number number number) (curry a b (prim +)))
  ((pi number number number) (literal (curry a b (prim +)))))

; === literals

(check-compiles #f ( boolean (literal #f)))
(check-compiles #t ( boolean (literal #t)))
(check-compiles 123 ( number (literal 123)))
(check-compiles 3.14 ( number (literal 3.14)))
(check-compiles 'foo ( symbol (literal 'foo)))
(check-compiles #\a ( char (literal #\a)))
(check-compiles "foo" ( string (literal "foo")))

; === globals

(check-compiles type ( type type))
(check-compiles boolean ( type boolean))
(check-compiles number ( type number))
(check-compiles string ( type string))

(check-compiles zero? ( (pi number boolean) zero?))
(check-compiles < ( (pi number number boolean) <))

; === application

(check-compiles
  (inc 10)
  (number (app inc (literal 10))))

(check-compiles
  ((+ 10) 20)
  (number (app (app + (literal 10)) (literal 20))))

(check-compiles
  (+ 10 20)
  (number (app (app + (literal 10)) (literal 20))))

(check-compile-raises (inc #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (number (literal 10)))

(check-compiles
  (let (x 10) (inc x))
  (number (let (x (literal 10)) (app inc x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (boolean
    (let (x (literal 10))
      (let (y (literal 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x number) (+ x 1))
    (zwiększ 10))
  (number
    (let
      (zwiększ (lambda x (app (app + x) (literal 1))))
      (app zwiększ (literal 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (number (literal 12)))

(check-compiles
  (lambda (i number) i)
  ((pi (i number) number) (lambda i i)))

(check-compiles
  (lambda (i number) (j number) (+ i j))
  (
    (pi (i number) (j number) number)
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  inc
  ((pi number number) inc))

(check-compiles
  (inc 1)
  (number (app inc (literal 1))))

(check-compiles
  (lambda (t type) t)
  ((pi (t type) type) (lambda t t)))

; === pi

(check-compiles
  (pi number boolean)
  ( type (pi number boolean)))

(check-compiles
  (pi (x type) x)
  (type (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (type (pi (x type) (pi (y type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (string (if (app zero? (literal 0)) (literal "zero") (literal "not-zero"))))

(check-compile-raises (if "not boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))

; === identity

(check-compiles
  (let
    (identity (lambda (t type) (lambda (x t) t)))
    ((identity number) 10))
  (type
    (let
      (identity (lambda t (lambda x t)))
      (app (app identity number) (literal 10)))))
