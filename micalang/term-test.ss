(import (except (micascheme) pi) (micalang term))

; term->datum

(check-term->datum (native 10) 10)
(check-term->datum (native "foo") "foo")
(check-term->datum (native 'inc) inc)

(check-term->datum
  (application (native 'foo) (native 'bar))
  (foo bar))

(check-term->datum
  (abstraction 'x (lambda (x) (apply-term zero? x)))
  (lambda (v0) (,zero? v0)))

(check-term->datum
  (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (term-apply (apply-term + x) y)))))
  (lambda (v0) (lambda (v1) ((,+ v0) v1))))

(check-term->datum
  (pi '_ (native 'nat) (lambda (_) (native 'nat)))
  (pi nat nat))

(check-term->datum
  (pi '_ (native 'nat) (lambda (_) (pi '_ (native 'string) (lambda (_) (native 'bool)))))
  (pi nat (pi string bool)))

(check-term->datum
  (pi 't (native 'nat) (lambda (t) (application (native 'inc) t)))
  (pi (v0 : nat) (inc v0)))

(check-term->datum
  (pi 'a (native 'nat) (lambda (a) (pi 'b (native 'string) (lambda (b) (application a b)))))
  (pi (v0 : nat) (pi (v1 : string) (v0 v1))))

(check-term->datum
  (conditional (variable 2) (native 10) (native 20))
  (if v2 10 20))

; --- term-apply

(check
  (equal?
    (term-apply (native (lambda (x) (native (zero? x)))) (native 0))
    (native #t)))

(check
  (equal?
    (term-apply
      (term-apply
        (native (lambda (x) (native (lambda (y) (native (- x y))))))
        (native 5))
      (native 3))
    (native 2)))

; --- term-equal?

(check (term-equal? (native 1) (native 1)))
(check (not (term-equal? (native 1) (native 2))))

(check
  (term-equal?
    (variable 3)
    (variable 3)))

(check
  (not
    (term-equal?
      (variable 3)
      (variable 4))))

(check
  (term-equal?
    (application (variable 3) (variable 3))
    (application (variable 3) (variable 3))))

(check
  (not
    (term-equal?
      (application (variable 3) (variable 3))
      (application (variable 3) (variable 4)))))

(check
  (term-equal?
    (abstraction 'x (lambda (x) (apply-term zero? x)))
    (abstraction 'x (lambda (x) (apply-term zero? x)))))

(check
  (term-equal?
    (abstraction 'x (lambda (x) (apply-term zero? x)))
    (abstraction 'y (lambda (y) (apply-term zero? y)))))

(check
  (not
    (term-equal?
      (abstraction 'x (lambda (x) (apply-term odd? x)))
      (abstraction 'x (lambda (x) (apply-term even? x))))))

(check
  (term-equal?
    (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (term-apply (apply-term + x) y)))))
    (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (term-apply (apply-term + x) y)))))))

(check
  (not
    (term-equal?
      (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (term-apply (apply-term + x) y)))))
      (abstraction 'x (lambda (x) (abstraction 'y (lambda (y) (term-apply (apply-term + y) x))))))))

(check
  (term-equal?
    (pi 'x (native 't1) (lambda (x) (apply-term zero? x)))
    (pi 'x (native 't1) (lambda (x) (apply-term zero? x)))))

(check
  (term-equal?
    (pi 'x (native 't1) (lambda (x) (apply-term zero? x)))
    (pi 'y (native 't1) (lambda (x) (apply-term zero? x)))))

(check
  (not
    (term-equal?
      (pi 'x (native 't1) (lambda (x) (apply-term zero? x)))
      (pi 'x (native 't2) (lambda (x) (apply-term zero? x))))))

(check
  (not
    (term-equal?
      (pi 'x (native 't1) (lambda (x) (apply-term zero? x)))
      (pi 'x (native 't1) (lambda (x) (apply-term one? x))))))

(check
  (term-equal?
    (pi 'x (native 't1) (lambda (x) (native 't2)))
    (pi #f (native 't1) (lambda (_) (native 't2)))))

(check
  (not
    (term-equal?
      (pi 'x (native 't1) (lambda (x) (native 't2)))
      (pi #f (native 't1) (lambda (_) (native 't3))))))





