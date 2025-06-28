(import (typico core lang))

(check-typico-works #f)
(check-typico-works #t)
(check-typico-works 1)
(check-typico-works #\a)
(check-typico-works "foo")

(check-typico-raises 1.0)
(check-typico-raises dupa)

(check-typico-equal? 1 1)
(check-typico-raises 1.0)
(check-typico-equal? (+ 1 2) 3)
(check-typico-raises (+ 1 "foo"))
