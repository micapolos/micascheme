(import (except (micascheme) pi) (micalang term))

; term->datum

(check-term->datum (native 10) 10)
(check-term->datum (native "foo") "foo")
(check-term->datum (native 'inc) inc)

(check-term->datum
  (application (native 'foo) (native 'bar))
  (foo bar))

(check-term->datum
  (abstraction (lambda (x) (term-apply (native 'inc) x)))
  (lambda (v0) (inc v0)))

(check-term->datum
  (abstraction (lambda (x) (abstraction (lambda (y) (term-apply (term-apply (native '+) x) y)))))
  (lambda (v0) (lambda (v1) ((+ v0) v1))))

(check-term->datum
  (pi (native 'nat) (lambda (_) (native 'nat)))
  (pi nat nat))

(check-term->datum
  (pi (native 'nat) (lambda (_) (pi (native 'string) (lambda (_) (native 'bool)))))
  (pi nat (pi string bool)))

(check-term->datum
  (pi (native 'nat) (lambda (t) (application (native 'inc) t)))
  (pi (v0 : nat) (inc v0)))

(check-term->datum
  (pi (native 'nat) (lambda (a) (pi (native 'string) (lambda (b) (application a b)))))
  (pi (v0 : nat) (pi (v1 : string) (v0 v1))))

(check-term->datum
  (branch (variable 2) (native 10) (native 20))
  (if v2 10 20))

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
    (abstraction (lambda (x) (term-apply (native zero?) x)))
    (abstraction (lambda (x) (term-apply (native zero?) x)))))

(check
  (not
    (term-equal?
      (abstraction (lambda (x) (term-apply (native odd?) x)))
      (abstraction (lambda (x) (term-apply (native even?) x))))))
