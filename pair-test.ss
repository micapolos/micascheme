(import (scheme) (check) (pair) (procedure))

; === null-or-pair? ===

(check-equal? (null-or-pair? `()) #t)
(check-equal? (null-or-pair? (cons 1 2)) #t)
(check-equal? (null-or-pair? 123) #f)

; === car-lets ===

(check-equal?
  (map-car (partial + 1) (cons 10 20))
  (cons 11 20))

(check-equal?
  (map-cdr (partial + 1) (cons 10 20))
  (cons 10 21))
