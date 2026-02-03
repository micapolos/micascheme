(import (except (micascheme) pi) (micalang type))

(check-type->datum 10 10)
(check-type->datum "foo" "foo")
(check-type->datum 'inc inc)

(check-type->datum
  (application 'foo 'bar)
  (foo bar))

(check-type->datum
  (lambda (x) (application 'inc x))
  (lambda (v0) (inc v0)))

(check-type->datum
  (pi 'nat (lambda (_) 'nat))
  (pi nat nat))

(check-type->datum
  (pi 'nat (lambda (_) (pi 'string (lambda (_) 'bool))))
  (pi nat (pi string bool)))

(check-type->datum
  (pi 'nat (lambda (t) (application 'inc t)))
  (pi (v0 : nat) (inc v0)))

(check-type->datum
  (pi 'nat (lambda (a) (pi 'string (lambda (b) (application a b)))))
  (pi (v0 : nat) (pi (v1 : string) (v0 v1))))

(check-type->datum
  (branch #t 10 20)
  (if #t 10 20))
