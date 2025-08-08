(import (scheme) (check) (core))

(define x '())
(cons! 10 x)
(check (equal? x '(10)))
(cons! 20 x)
(check (equal? x '(20 10)))
(push! x 30)
(check (equal? x '(30 20 10)))

(set! x 0)
(check (equal? x 0))
(set+! x 10)
(check (equal? x 10))
(set-! x 1)
(check (equal? x 9))
(set*! x 2)
(check (equal? x 18))

(check (equal? (cond?) #f))
(check (equal? (cond? (#t "foo")) "foo"))
(check (equal? (cond? (#f "foo")) #f))
(check (equal? (cond? (#f "foo") (#t "bar")) "bar"))
(check (equal? (cond? (#f "foo") (#f "bar")) #f))

(comment
  (This is a comment)
  (Do you like it?))
