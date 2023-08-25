(import (micascheme) (value))

; value-tuple

(check (equal? (value-tuple 0 #f) (list)))
(check (equal? (value-tuple 1 `a) (list `a)))
(check (equal? (value-tuple 2 (cons `a `b)) (list `a `b)))
(check (equal? (value-tuple 3 (vector `a `b `c)) (list `a `b `c)))

; value-index

(check (equal? (value-index 1 #f) 0))
(check (equal? (value-index 2 #t) 0))
(check (equal? (value-index 2 #f) 1))
(check (equal? (value-index 3 0) 0))
(check (equal? (value-index 3 1) 1))
(check (equal? (value-index 3 2) 2))

; value-indexed

(check (equal? (value-indexed 1 `a) (cons 0 `a)))
(check (equal? (value-indexed 2 (cons #t `a)) (cons 0 `a)))
(check (equal? (value-indexed 2 (cons #f `b)) (cons 1 `b)))
(check (equal? (value-indexed 3 (cons 0 `a)) (cons 0 `a)))
(check (equal? (value-indexed 3 (cons 1 `b)) (cons 1 `b)))
(check (equal? (value-indexed 3 (cons 2 `c)) (cons 2 `c)))
