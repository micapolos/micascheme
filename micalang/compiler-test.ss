(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === type

(check-compiles type (compiled type type type))

; === native

(check-compiles
  (native boolean foo)
  (compiled boolean boolean (native foo)))

(check-compiles
  (native (pi number number number) (%lambda (x) (%lambda (y) (%+ x y))))
  (compiled
    (pi number number number)
    (pi number (pi number number))
    (native (%lambda (x) (%lambda (y) (%+ x y))))))

; === natives

(check-compiles #f (compiled boolean boolean (native #f)))
(check-compiles #t (compiled boolean boolean (native #t)))
(check-compiles 123 (compiled number number (native 123)))
(check-compiles 3.14 (compiled number number (native 3.14)))
(check-compiles 'foo (compiled symbol symbol (native 'foo)))
(check-compiles #\a (compiled char char (native #\a)))
(check-compiles "foo" (compiled string string (native "foo")))

; === globals

(check-compiles boolean (compiled type type boolean))
(check-compiles number (compiled type type number))
(check-compiles string (compiled type type string))

(check-compiles
  zero?
  (compiled
    (pi number boolean)
    (pi number boolean)
    zero?))

(check-compiles
  <
  (compiled
    (pi number number boolean)
    (pi number number boolean)
    <))

; === application

(check-compiles
  (inc 10)
  (compiled
    number
    number
    (app inc (native 10))))

(check-compiles
  ((+ 10) 20)
  (compiled
    number
    number
    (app (app + (native 10)) (native 20))))

(check-compiles
  (+ 10 20)
  (compiled
    number
    number
    (app (app + (native 10)) (native 20))))

(check-compile-raises (inc #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (compiled number number (native 10)))

(check-compiles
  (let (x 10) (inc x))
  (compiled number number (let (x (native 10)) (app inc x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (compiled
    boolean
    boolean
    (let (x (native 10))
      (let (y (native 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x number) (+ x 1))
    (zwiększ 10))
  (compiled
    number
    number
    (let
      (zwiększ (lambda x (app (app + x) (native 1))))
      (app zwiększ (native 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (compiled number number (native 12)))

(check-compiles
  (lambda (i number) i)
  (compiled
    (pi (i number) number)
    (pi (i number) number)
    (lambda i i)))

(check-compiles
  (lambda (i number) (j number) (+ i j))
  (compiled
    (pi (i number) (j number) number)
    (pi (i number) (pi (j number) number))
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  inc
  (compiled
    (pi number number)
    (pi number number)
    inc))

(check-compiles
  (inc 1)
  (compiled
    number
    number
    (app inc (native 1))))

(check-compiles
  (lambda (t type) t)
  (compiled
    (pi (t type) type)
    (pi (t type) type)
    (lambda t t)))

; === pi

(check-compiles
  (pi number boolean)
  (compiled
    type
    type
    (pi number boolean)))

(check-compiles
  (pi (x type) x)
  (compiled
    type
    type
    (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (compiled
    type
    type
    (pi (x type) (pi (y type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (compiled
    string
    string
    (if (app zero? (native 0)) (native "zero") (native "not-zero"))))

(check-compile-raises (if "not boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))

; === identity

(check-compiles
  (let
    (identity (lambda (t type) (lambda (x t) t)))
    ((identity number) 10))
  (compiled
    type
    type
    (let
      (identity (lambda t (lambda x t)))
      (app (app identity number) (native 10)))))

