(import (micalang base) (micalang term) (micalang reify))

(check-reify (native 'a) a)

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
  (abstraction 'x (lambda (x) x))
  (lambda (x type) x))

(check-reify
  (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (application x y)))))
  (lambda (x type) (y type) (x y)))

(check-reify
  (pi #f (native 't1) (lambda (_) (native 't2)))
  (pi t1 t2))

(check-reify
  (pi 'x (native 'number) (lambda (x) x))
  (pi (x number) x))

(check-reify
  (pi 'x (native 'number) (lambda (x) (pi 'y (native 'bool) (lambda (y) (application x y)))))
  (pi (x number) (y bool) (x y)))

