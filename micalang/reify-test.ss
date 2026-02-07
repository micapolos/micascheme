(import (micalang base) (micalang term) (micalang reify))

(check-reify (native 'a) a)

(check-reify
  (conditional (native 'cond) (native 't) (native 'f))
  (if cond t f))

(check-reify
  (application (native 'a) (native 'b))
  (a b))

(check-reify
  (abstraction (lambda (x) x))
  (lambda (v0 type) v0))

(check-reify
  (abstraction (lambda (x) (abstraction (lambda (y) (application x y)))))
  (lambda (v0 type) (lambda (v1 type) (v0 v1))))

(check-reify
  (pi (native 'int) (lambda (x) x))
  (pi (v0 int) v0))

(check-reify
  (pi (native 'int) (lambda (x) (pi (native 'bool) (lambda (y) (application x y)))))
  (pi (v0 int) (pi (v1 bool) (v0 v1))))

