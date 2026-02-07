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
  (native (pi int int int) (curry a b (prim +)))
  (typed (pi int (pi int int)) (literal (curry a b (prim +)))))

; === literals

(check-compiles #f (typed bool (literal #f)))
(check-compiles #t (typed bool (literal #t)))
(check-compiles 123 (typed int (literal 123)))
(check-compiles 'foo (typed symbol (literal 'foo)))
(check-compiles "foo" (typed string (literal "foo")))

(check-compile-raises 123123123123123123123123)
(check-compile-raises #\a)

; === globals

(check-compiles type (typed type type))
(check-compiles bool (typed type bool))
(check-compiles int (typed type int))
(check-compiles string (typed type string))

(check-compiles zero? (typed (pi int bool) zero?))
(check-compiles < (typed (pi int (pi int bool)) <))

; === application

(check-compiles
  (inc 10)
  (typed int (app inc (literal 10))))

(check-compiles
  ((+ 10) 20)
  (typed int (app (app + (literal 10)) (literal 20))))

(check-compiles
  (+ 10 20)
  (typed int (app (app + (literal 10)) (literal 20))))

(check-compile-raises (inc #t))
(check-compile-raises (+ 1 #t))
(check-compile-raises (+ #t 2))

; === let

(check-compiles
  (let 10)
  (typed int (literal 10)))

(check-compiles
  (let (x 10) (inc x))
  (typed int (let (x (literal 10)) (app inc x))))

(check-compiles
  (let (x 10) (y 20) (< x y))
  (typed bool
    (let (x (literal 10))
      (let (y (literal 20))
        (app (app < x) y)))))

(check-compiles
  (let
    (zwiększ (x int) (inc x))
    (zwiększ 10))
  (typed int
    (let
      (zwiększ (lambda x (app inc x)))
      (app zwiększ (literal 10)))))

; === lambda

(check-compiles
  (lambda 12)
  (typed int (literal 12)))

(check-compiles
  (lambda (i int) i)
  (typed (pi int int) (lambda i i)))

(check-compiles
  (lambda (i int) (j int) (+ i j))
  (typed
    (pi int (pi int int))
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  inc
  (typed (pi int int) inc))

(check-compiles
  (inc 1)
  (typed int (app inc (literal 1))))

; === pi

(check-compiles
  (pi int bool)
  (typed type (pi int bool)))

(check-compiles
  (pi (x type) x)
  (typed type (pi (x type) x)))

(check-compiles
  (pi (x type) (y type) x)
  (typed type (pi (x type) (pi (y type) x))))
