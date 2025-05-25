(import (scheme) (check) (core))

(define x '())
(cons! 10 x)
(check (equal? x '(10)))
(cons! 20 x)
(check (equal? x '(20 10)))
