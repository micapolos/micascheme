(import
  (micalang base)
  (micalang term)
  (micalang typed)
  (micalang compiler))

; === native

(check-compiles
  (native bool (prim +))
  (typed bool (literal (prim +))))

(check-compiles
  (native (pi number number number) (curry a b (prim +)))
  (typed (pi number number number) (literal (curry a b (prim +)))))

; === literals

(check-compiles #f (typed bool (literal #f)))
(check-compiles #t (typed bool (literal #t)))
(check-compiles 123 (typed number (literal 123)))
(check-compiles 3.14 (typed number (literal 3.14)))
(check-compiles 'foo (typed symbol (literal 'foo)))
(check-compiles "foo" (typed string (literal "foo")))

(check-compile-raises #\a)

; === globals

(check-compiles type (typed type type))
(check-compiles bool (typed type bool))
(check-compiles number (typed type number))
(check-compiles string (typed type string))

(check-compiles zero? (typed (pi number bool) zero?))
(check-compiles < (typed (pi number number bool) <))

; === application

(check-compiles
  (inc 10)
  (typed number (app inc (literal 10))))

(check-compiles
  ((+ 10) 20)
  (typed number (app (app + (literal 10)) (literal 20))))

(check-compiles
  (+ 10 20)
  (typed number (app (app + (literal 10)) (literal 20))))

(check-compile-raises (inc #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (typed number (literal 10)))

(check-compiles
  (let (x 10) (inc x))
  (typed number (let (x (literal 10)) (app inc x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (typed bool
    (let (x (literal 10))
      (let (y (literal 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x number) (inc x))
    (zwiększ 10))
  (typed number
    (let
      (zwiększ (lambda x (app inc x)))
      (app zwiększ (literal 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (typed number (literal 12)))

; TODO: this is wrong: 'id should be 'i
(check-compiles
  (lambda (i number) i)
  (typed (pi (id number) number) (lambda i i)))

; TODO: this is wrong: 'id should be 'i and 'j
(check-compiles
  (lambda (i number) (j number) (+ i j))
  (typed
    (pi (id number) (id number) number)
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  inc
  (typed (pi number number) inc))

(check-compiles
  (inc 1)
  (typed number (app inc (literal 1))))

; === pi

(check-compiles
  (pi number bool)
  (typed type (pi number bool)))

(check-compiles
  (pi (x type) x)
  (typed type (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (typed type (pi (x type) (pi (y type) x))))

; === if

(check-compiles
  (if (zero? 0) "zero" "not-zero")
  (typed string (if (app zero? (literal 0)) (literal "zero") (literal "not-zero"))))

(check-compile-raises (if "not-boolean" "zero" "not-zero"))
(check-compile-raises (if #t 10 "not-number"))
