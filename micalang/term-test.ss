(import (except (micascheme) pi) (micalang term))

(check-term->datum 10 10)
(check-term->datum "foo" "foo")
(check-term->datum 'inc inc)

(check-term->datum
  (application 'foo 'bar)
  (foo bar))

(check-term->datum
  (lambda (x) (application 'inc x))
  (lambda (v0) (inc v0)))

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
