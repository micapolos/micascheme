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

; --- term-equal?

(check (term-equal? type type))
(check (not (term-equal? type (native type))))

(check (term-equal? (native 1) (native 1)))
(check (not (term-equal? (native 1) (native 2))))

(check
  (term-equal?
    (variable 'x)
    (variable 'x)))

(check
  (not
    (term-equal?
      (variable 'x)
      (variable 'y))))

(check
  (term-equal?
    (application (variable 'x) (variable 'x))
    (application (variable 'x) (variable 'x))))

(check
  (not
    (term-equal?
      (application (variable 'x) (variable 'x))
      (application (variable 'x) (variable 'y)))))

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
