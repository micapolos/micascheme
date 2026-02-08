(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === type

(check-compiles type (type type))

; === native

(check-compiles
  (native boolean foo)
  (boolean (native foo)))

(check-compiles
  (native (pi number number number) (%lambda (x) (%lambda (y) (%+ x y))))
  ((pi number number number) (native (%lambda (x) (%lambda (y) (%+ x y))))))

; === natives

(check-compiles #f (boolean (native #f)))
(check-compiles #t (boolean (native #t)))
(check-compiles 123 (number (native 123)))
(check-compiles 3.14 (number (native 3.14)))
(check-compiles 'foo (symbol (native 'foo)))
(check-compiles #\a (char (native #\a)))
(check-compiles "foo" (string (native "foo")))

; === globals

(check-compiles type (type type))
(check-compiles boolean (type boolean))
(check-compiles number (type number))
(check-compiles string (type string))

(check-compiles zero? ((pi number boolean) zero?))
(check-compiles < ((pi number number boolean) <))

; === application

(check-compiles
  (inc 10)
  (number (app inc (native 10))))

(check-compiles
  ((+ 10) 20)
  (number (app (app + (native 10)) (native 20))))

(check-compiles
  (+ 10 20)
  (number (app (app + (native 10)) (native 20))))

(check-compile-raises (inc #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (number (native 10)))

(check-compiles
  (let (x 10) (inc x))
  (number (let (x (native 10)) (app inc x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (boolean
    (let (x (native 10))
      (let (y (native 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x number) (+ x 1))
    (zwiększ 10))
  (number
    (let
      (zwiększ (lambda x (app (app + x) (native 1))))
      (app zwiększ (native 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (number (native 12)))

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
  (number (app inc (native 1))))

(check-compiles
  (lambda (t type) t)
  ((pi (t type) type) (lambda t t)))

; === pi

(check-compiles
  (pi number boolean)
  (type (pi number boolean)))

(check-compiles
  (pi (x type) x)
  (type (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (type (pi (x type) (pi (y type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (string (if (app zero? (native 0)) (native "zero") (native "not-zero"))))

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
      (app (app identity number) (native 10)))))
