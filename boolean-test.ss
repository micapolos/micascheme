(import (check) (boolean))

; === false? ===

(check (equal? (false? #f) #t))
(check (equal? (false? #t) #f))
(check (equal? (false? 123) #f))

(check (equal? (true? #f) #f))
(check (equal? (true? #t) #t))
(check (equal? (true? 123) #t))
