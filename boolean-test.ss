(import (check) (boolean))

; === false? ===

(check (equal? (false? #f) #t))
(check (equal? (false? #t) #f))
(check (equal? (false? 123) #f))

(check (equal? (not-false? #f) #f))
(check (equal? (not-false? #t) #t))
(check (equal? (not-false? 123) #t))
