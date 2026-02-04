(import
  (micalang base)
  (micalang term)
  (micalang compiler))

; === int

(check-compiles 123 (typed int (literal 123)))
(check-compile-raises 123123123123123123123123)

; === variable

(check-compiles
  (x1 (typed (native 't1) 'v1))
  (x2 (typed (native 't2) 'v2))
  x1
  (typed t1 v1))

(check-compiles
  (x1 (typed (native 't1) 'v1))
  (x2 (typed (native 't2) 'v2))
  x2
  (typed t2 v2))

(check-compile-raises
  (x1 (typed (native 't1) 'v1))
  (x2 (typed (native 't2) 'v2))
  x3)

; === lambda

(check-compiles
  (int (typed (native 'type) 'int))
  (lambda (i : int) i)
  (typed (pi int int) (lambda (i) i)))

(check-compiles
  (inc (typed (pi (native 'int) (lambda (_) (native 'int))) 'inc))
  inc
  (typed (pi int int) inc))

(check-compiles
  (inc (typed (pi (native 'int) (lambda (_) (native 'int))) 'inc))
  (inc 1)
  (typed int (app inc (literal 1))))
