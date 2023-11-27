(import (check) (pair))

; === unpair

(check
  (=
    (unpair (cons 3 2) l r (- l r))
    1))

; === pair-values ===

(lets
  ((values $car $cdr) (pair-values (cons 1 2)))
  (begin
    (check (equal? $car 1))
    (check (equal? $cdr 2))))

; === null-or-pair? ===

(check (equal? (null-or-pair? `()) #t))
(check (equal? (null-or-pair? (cons 1 2)) #t))
(check (equal? (null-or-pair? 123) #f))