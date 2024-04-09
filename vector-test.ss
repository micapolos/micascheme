(import (scheme) (check) (vector))

(check (equal? (build-immutable-vector 3 add1) (vector 1 2 3)))
(check (equal? (immutable-vector? (build-immutable-vector 3 add1)) #t))

(check (equal? (build-vector 3 add1) (vector 1 2 3)))
(check (equal? (immutable-vector? (build-vector 3 add1)) #f))
