(import (check) (pair))

; === null-or-pair? ===

(check (equal? (null-or-pair? `()) #t))
(check (equal? (null-or-pair? (cons 1 2)) #t))
(check (equal? (null-or-pair? 123) #f))
