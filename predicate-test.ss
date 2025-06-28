(import (scheme) (check) (predicate))

(check (equal? ((or? string? integer?) "foo") #t))
(check (equal? ((or? string? integer?) 123) #t))
(check (equal? ((or? string? integer?) #\a) #f))

(check (equal? ((and? integer? exact?) "foo") #f))
(check (equal? ((and? integer? exact?) 1.0) #f))
(check (equal? ((and? integer? exact?) 1) #t))
