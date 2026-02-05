(import
  (micalang base)
  (micalang term)
  (micalang typed)
  (micalang compiler))

; === int

(check-compiles 123 (typed int (literal 123)))
(check-compiles #f (typed bool (literal #f)))
(check-compile-raises 123123123123123123123123)

; === globals

(check-compiles type (typed type type))
(check-compiles bool (typed type bool))
(check-compiles int (typed type int))
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

; === lambda

(check-compiles
  (lambda (i : int) i)
  (typed (pi int int) (lambda i i)))

(check-compiles
  (lambda (i : int) (j : int) (+ i j))
  (typed
    (pi int (pi int int))
    (lambda i (lambda j (app (app + i) j)))))

(check-compiles
  inc
  (typed (pi int int) inc))

(check-compiles
  (inc 1)
  (typed int (app inc (literal 1))))
