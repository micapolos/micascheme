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

(check-typico-equal?
  (begin
    (define hello-world
      (begin
        (define hello "Hello")
        (define world "world")
        (append hello ", " world "!")))
    (append hello-world " (" (string (length hello-world)) ")"))
  "Hello, world! (13)")
