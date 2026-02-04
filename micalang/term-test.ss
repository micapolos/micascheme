(import (except (micascheme) pi) (micalang term))

; term->datum

(check-term->datum 10 10)
(check-term->datum "foo" "foo")
(check-term->datum 'inc inc)

(check-term->datum
  (application 'foo 'bar)
  (foo bar))

(check-term->datum
  (abstraction (lambda (x) (term-apply 'inc x)))
  (lambda (v0) (inc v0)))

(check-term->datum
  (abstraction (lambda (x) (abstraction (lambda (y) (term-apply (term-apply '+ x) y)))))
  (lambda (v0) (lambda (v1) ((+ v0) v1))))

(check-term->datum
  (pi 'nat (lambda (_) 'nat))
  (pi nat nat))

(check-term->datum
  (pi 'nat (lambda (_) (pi 'string (lambda (_) 'bool))))
  (pi nat (pi string bool)))

(check-term->datum
  (pi 'nat (lambda (t) (application 'inc t)))
  (pi (v0 : nat) (inc v0)))

(check-term->datum
  (pi 'nat (lambda (a) (pi 'string (lambda (b) (application a b)))))
  (pi (v0 : nat) (pi (v1 : string) (v0 v1))))

(check-term->datum
  (branch #t 10 20)
  (if #t 10 20))

; --- term-equal?

(check (term-equal? 1 1))
(check (term-equal? 'foo 'foo))
(check (term-equal? even? even?))

(check (not (term-equal? odd? even?)))

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
    (abstraction (lambda (x) (term-apply zero? x)))
    (abstraction (lambda (x) (term-apply zero? x)))))

(check
  (not
    (term-equal?
      (abstraction (lambda (x) (term-apply odd? x)))
      (abstraction (lambda (x) (term-apply even? x))))))
