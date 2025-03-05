(import (micascheme) (typed lang) (typed type))

(check (equal? (tt (assume 'type 'value)) 'value))
(check (equal? (tt (as 123 any-fixnum)) 123))
(check (equal? (tt 123) 123))
(check (equal? (tt ((lambda () 123))) 123))
(check (equal? (tt ((lambda ((any-fixnum fx)) fx) 123)) 123))
