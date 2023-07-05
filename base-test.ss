(import (chezscheme) (base))

(assert (equal? (associ (list) 100 `a) #f))
(assert (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `a) (cons 100 `foo)))
(assert (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `b) (cons 101 `bar)))
(assert (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `c) #f))
