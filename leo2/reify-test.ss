(import (leo2 base) (leo2 term) (leo2 reify))

(check-reify (type 0) a-type)
(check-reify (type 1) a-type-type)
(check-reify (type 2) a-type-type-type)

(check-reify (variable 'x) x)

(check-reify
  (abstraction (lambda (x) x))
  (lambda v0 v0))

(check-reify
  (abstraction (lambda (x)
    (abstraction (lambda (y)
      (application x y)))))
  (lambda v0 v1 (v0 v1)))

(check-reify
  (abstraction (lambda (x)
    (abstraction (lambda (y)
      (abstraction (lambda (z)
        (application (application x y) z)))))))
  (lambda v0 v1 v2 (v0 v1 v2)))

(check-reify
  (abstraction-type (type 0) (lambda (x) (application (native 'list) x)))
  (a-lambda (v0 : a-type) (list v0)))

(check-reify
  (abstraction-type (type 0) (lambda (x)
    (abstraction-type (native 'a-number) (lambda (y)
      (application (application (native 'list) x) y)))))
  (a-lambda (v0 : a-type) (v1 : a-number) (list v0 v1)))

(check-reify
  (recursion (lambda (fn) fn))
  (recursive lambda v0 v0))

(check-reify
  (recursion (lambda (fn)
    (abstraction (lambda (n)
      (application fn n)))))
   (recursive lambda v0 v1 (v0 v1)))

(check-reify
  (branch (variable 'x) (variable 'y) (variable 'z))
  (if x y z))
