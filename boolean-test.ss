(import (check) (boolean))

; === false? ===

(check (equal? (false? #f) #t))
(check (equal? (false? #t) #f))
(check (equal? (false? 123) #f))
