(import (scheme) (core) (check) (scoped) (lets) (procedure))

(define destroyed (list))

(define-scoped scoped+
  (lambda $xs (apply + $xs))
  (lambda ($sum) (cons! `(+ ,$sum) destroyed)))

(define-scoped (scoped* . $xs)
  ($product (apply * $xs))
  (cons! `(* ,$product) destroyed))

(check (equal? destroyed '()))

(lets
  ($sum (scoped+ 2 3))
  ($product (scoped* 2 3))
  (run
    (check (equal? $sum 5))
    (check (equal? $product 6))
    (check (equal? destroyed '()))))

(check (equal? destroyed '((+ 5) (* 6))))
