(import (micascheme) (typed lang-2) (typed typed) (any))

(check (equal? (tt (assume any-string "foo")) "foo"))
(check (equal? (tt any-string) any-string))
(check (equal? (tt "foo") "foo"))
