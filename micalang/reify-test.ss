(import (micalang base) (micalang term) (micalang reify))

(check (equal? (unique-id '(a b b-0) #f 'v 0) 'v))
(check (equal? (unique-id '(a b b-0 v) #f 'v 0) 'v-0))
(check (equal? (unique-id '(a b b-0 v v-0 v) #f 'v 0) 'v-1))
(check (equal? (unique-id '(a b b-0 v v-0 v-1) #f 'v 0) 'v-2))
(check (equal? (unique-id '(a b b-0) 'a 'a 0) 'a-0))
(check (equal? (unique-id '(a b b-0) 'b 'b 0) 'b-1))
(check (equal? (unique-id '(a b b-0) 'c 'c 0) 'c))

(check-reify a-type a-type)

(check-reify (native #t) #t)
(check-reify (native 1) 1)
(check-reify (native #\a) #\a)
(check-reify (native "foo") "foo")
(check-reify (native 'a) a)

(check-reify (variable 0) v0)
(check-reify (variable 1) v1)

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
  (abstraction 'x (native 'int)
    (lambda (a)
      (abstraction 'x (native 'int)
        (lambda (b) (application a b)))))
  (lambda (x int) (x-0 int) (x x-0)))

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

