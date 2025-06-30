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
        (comma-separated (=> (s1 string) (s2 string) (append s1 ", " s2)))
        (exclamated (=> (s string) (append s "!")))
        (exclamated (comma-separated hello world))))
    (in-parentheses (=> (s string) (append "(" s ")")))
    (space-separated (=> (s1 string) (s2 string) (append s1 " " s2)))
    (space-separated hello-world (in-parentheses (string (length hello-world)))))
  "Hello, world! (13)")
