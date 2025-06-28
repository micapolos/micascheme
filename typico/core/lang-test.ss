(import (typico core lang))

(check-typico-works 0)
(check-typico-raises dupa)

(check-typico-equal? 123 123)
(check-typico-equal? (+ 1 2) 3)
(check-typico-raises (+ 1 "foo"))
