(import (except (micascheme) pi) (micalang term))

; --- term-apply

(check
  (equal?
    (term-apply (native zero?) (native 0))
    (native #t)))

(check
  (equal?
    (term-apply
      (term-apply
        (native (lambda (x) (lambda (y) (- x y))))
        (native 5))
      (native 3))
    (native 2)))

(check
  (equal?
    (term-apply
      (native (lambda (x) (+ x 1)))
      (tagged (constant 'fx) (native 5)))
    (native 6)))

(check
  (equal?
    (term-apply
      (tagged (constant 'tag) (native (lambda (x) (+ x 1))))
      (native 5))
    (native 6)))

; --- term=?

(check (term=? a-type a-type))
(check (not (term=? a-type (native a-type))))

(check (term=? (native 1) (native 1)))
(check (not (term=? (native 1) (native 2))))

(check
  (term=?
    (variable 0)
    (variable 0)))

(check
  (not
    (term=?
      (variable 0)
      (variable 1))))

(check
  (term=?
    (constant '(this is my (constant)))
    (constant '(this is my (constant)))))

(check
  (not
    (term=?
      (constant `(this is my (constant)))
      (constant `(this is other (constant))))))

(check
  (term=?
    (tagged (constant 'my-tag) (native 10))
    (tagged (constant 'my-tag) (native 10))))

(check
  (not
    (term=?
      (tagged (constant 'my-tag) (native 10))
      (tagged (constant 'other-tag) (native 10)))))

(check
  (not
    (term=?
      (tagged (constant 'my-tag) (native 10))
      (tagged (constant 'my-tag) (native 20)))))

(check
  (term=?
    (application (variable 0) (variable 0))
    (application (variable 0) (variable 0))))

(check
  (not
    (term=?
      (application (variable 0) (variable 0))
      (application (variable 0) (variable 1)))))

(check
  (term=?
    (abstraction 'x (native 'int) (lambda (x) (apply-term zero? x)))
    (abstraction 'x (native 'int) (lambda (x) (apply-term zero? x)))))

(check
  (term=?
    (abstraction 'x (native 'int) (lambda (x) (apply-term zero? x)))
    (abstraction 'y (native 'int) (lambda (y) (apply-term zero? y)))))

(check
  (not
    (term=?
      (abstraction 'x (native 'int) (lambda (x) (apply-term zero? x)))
      (abstraction 'y (native 'bool) (lambda (y) (apply-term zero? y))))))

(check
  (not
    (term=?
      (abstraction 'x (native 'int) (lambda (x) (apply-term odd? x)))
      (abstraction 'x (native 'int) (lambda (x) (apply-term even? x))))))

(check
  (term=?
    (abstraction 'x (native 'int) (lambda (x) (abstraction 'y (native 'int) (lambda (y) (term-apply (apply-term + x) y)))))
    (abstraction 'x (native 'int) (lambda (x) (abstraction 'y (native 'int) (lambda (y) (term-apply (apply-term + x) y)))))))

(check
  (not
    (term=?
      (abstraction 'x (native 'int) (lambda (x) (abstraction 'y (native 'int) (lambda (y) (term-apply (apply-term + x) y)))))
      (abstraction 'x (native 'int) (lambda (x) (abstraction 'y (native 'int) (lambda (y) (term-apply (apply-term + y) x))))))))

(check
  (term=?
    (type-abstraction 'x (native 't1) (lambda (x) (apply-term zero? x)))
    (type-abstraction 'x (native 't1) (lambda (x) (apply-term zero? x)))))

(check
  (term=?
    (type-abstraction 'x (native 't1) (lambda (x) (apply-term zero? x)))
    (type-abstraction 'y (native 't1) (lambda (x) (apply-term zero? x)))))

(check
  (not
    (term=?
      (type-abstraction 'x (native 't1) (lambda (x) (apply-term zero? x)))
      (type-abstraction 'x (native 't2) (lambda (x) (apply-term zero? x))))))

(check
  (not
    (term=?
      (type-abstraction 'x (native 't1) (lambda (x) (apply-term zero? x)))
      (type-abstraction 'x (native 't1) (lambda (x) (apply-term one? x))))))

(check
  (term=?
    (type-abstraction 'x (native 't1) (lambda (x) (native 't2)))
    (type-abstraction #f (native 't1) (lambda (_) (native 't2)))))

(check
  (not
    (term=?
      (type-abstraction 'x (native 't1) (lambda (x) (native 't2)))
      (type-abstraction #f (native 't1) (lambda (_) (native 't3))))))

(lets
  ($procedure1 (lambda ($compiler $term) 'foo))
  ($procedure2 (lambda ($compiler $term) 'bar))
  (run
    (check (term=? (macro $procedure1) (macro $procedure1)))
    (check (not (term=? (macro $procedure1) (macro $procedure2))))))
