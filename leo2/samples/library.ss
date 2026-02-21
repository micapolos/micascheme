(this stop "!")
(this separator ", ")
(this lucky 7)

(this doubly lucky
  (add
    (the lucky number)
    (the lucky number)))

(this greet
  (with
    (any string)
    (do
      (append
        "Hello"
        (the separator string)
        (the string)
        (the stop string (default "!"))))))

(this hello world (greet "world"))

(this hello hell (greet "hell" (stop "?")))

(this increment
  (with
    (any number)
    (do (add (the number) 1))))

(this decrement
  (with
    (any number)
    (do (subtract (the number) 1))))
