(import (leo2 base) (leo2 term) (leo2 dependent))

(check
  (abstraction-dependent?
    (abstraction (lambda (x) x))))

(check
  (not
    (abstraction-dependent?
      (abstraction (lambda (x) (native 1))))))

(check
  (abstraction-type-dependent?
    (abstraction-type (variable 't) (lambda (x) x))))

(check
  (not
    (abstraction-type-dependent?
      (abstraction-type (variable 't) (lambda (x) (native 1))))))

(check
  (recursion-dependent?
    (recursion (lambda (x) x))))

(check
  (not
    (recursion-dependent?
      (recursion (lambda (x) (native 1))))))

