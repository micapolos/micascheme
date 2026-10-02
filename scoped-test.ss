(import (scheme) (core) (check) (scoped) (lets) (procedure))

(define destroyed (list))

(define-scoped (scoped+ . $xs)
  ($sum (apply + $xs))
  (cons! `(+ ,$sum) destroyed))

(check (equal? destroyed '()))

(lets
  ($sum1 (scoped+ 2 2))
  ($sum2 (scoped+ 5 7))
  (run
    (check (equal? $sum1 4))
    (check (equal? $sum2 12))
    (check (equal? destroyed '()))))

(check (equal? destroyed '((+ 4) (+ 12))))
