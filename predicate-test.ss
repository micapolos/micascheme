(import (scheme) (check) (predicate))

(check (equal? ((or-predicate string? integer?) "foo") #t))
(check (equal? ((or-predicate string? integer?) 123) #t))
(check (equal? ((or-predicate string? integer?) #\a) #f))
