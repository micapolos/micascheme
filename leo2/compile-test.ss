(import (leo2 base) (leo2 term) (leo2 compile))

(check-compiles
  (variable 'x)
  x)

(check-compiles
  (native "foo")
  "foo")

(check-compiles
  (native-application 'string-append (list (native "foo") (variable 'x)))
  (string-append "foo" x))

(check-compiles
  (application (variable 'x) (variable 'y))
  (x y))

(check-compiles
  (abstraction (lambda (x) x))
  (lambda (v0) v0))

(check-compiles
  (abstraction (lambda (x) (abstraction (lambda (y) (application x y)))))
  (lambda (v0) (lambda (v1) (v0 v1))))

(check-compiles
  (recursion (lambda (f) (abstraction (lambda (x) (application f x)))))
  (letrec ((v0 (lambda (v1) (v0 v1)))) v0))

(check-compiles
  (branch (variable 'x) (variable 'y) (variable 'z))
  (if x y z))
