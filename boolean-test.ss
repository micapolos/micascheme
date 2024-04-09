(import (scheme) (check) (boolean))

; === false? ===

(check (equal? (false? #f) #t))
(check (equal? (false? #t) #f))
(check (equal? (false? 123) #f))

(check (equal? (not-false? #f) #f))
(check (equal? (not-false? #t) #t))
(check (equal? (not-false? 123) #t))

(check (equal? (xor) #f))

(check (equal? (xor #f) #f))
(check (equal? (xor #t) #t))

(check (equal? (xor #f #f) #f))
(check (equal? (xor #f #t) #t))
(check (equal? (xor #t #f) #t))
(check (equal? (xor #t #t) #f))

(check (equal? (xor #f #f #f) #f))
(check (equal? (xor #f #f #t) #t))
(check (equal? (xor #f #t #f) #t))
(check (equal? (xor #f #t #t) #f))
(check (equal? (xor #t #f #f) #t))
(check (equal? (xor #t #f #t) #f))
(check (equal? (xor #t #t #f) #f))
(check (equal? (xor #t #t #t) #f))
