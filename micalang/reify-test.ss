(import (micalang base) (micalang term) (micalang reify))

(check-reify a-type a-type)

(check-reify (native #t) #t)
(check-reify (native 1) 1)
(check-reify (native #\a) #\a)
(check-reify (native "foo") "foo")
(check-reify (native 'a) a)

(check-reify (variable 'zero?) zero?)

(check-reify
  (constant '(this is my (constant)))
  (this is my (constant)))

(check-reify
  (tagged (constant 'fx) (native 10))
  (fx 10))

(check-reify
  (tagged (constant 'list) (tagged (constant 'fx) (native 10)))
  (list (fx 10)))

(check-reify
  (conditional (native 'cond) (native 't) (native 'f))
  (if cond t f))

(check-reify
  (application (native 'a) (native 'b))
  (a b))

(check-reify
  (application (application (native 'a) (native 'b)) (native 'c))
  (a b c))

(check-reify
  (abstraction 'x (native 'int) (lambda (x) x))
  (lambda (x int) x))

(check-reify
  (abstraction 'x (native 'int) (lambda (x) (abstraction 'y (native 'bool) (lambda (y) (application x y)))))
  (lambda (x int) (y bool) (x y)))

(check-reify
  (type-abstraction #f (native 't1) (lambda (_) (native 't2)))
  (a-lambda t1 t2))

(check-reify
  (type-abstraction 'x (native 'number) (lambda (x) x))
  (a-lambda (x number) x))

(check-reify
  (type-abstraction 'x (native 'number) (lambda (x) (type-abstraction 'y (native 'boolean) (lambda (y) (application x y)))))
  (a-lambda (x number) (y boolean) (x y)))

