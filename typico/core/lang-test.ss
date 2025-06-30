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
  (let
    (hello-world
      (let
        (hello "Hello")
        (world "world")
        (dodaj (=> (s1 string) (s2 string) (s3 string) (s4 string) (append s1 s2 s3 s4)))
        (dodaj hello ", " world "!")))
    (append hello-world " " "(" (string (length hello-world)) ")"))
  "Hello, world! (13)")
