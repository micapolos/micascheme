(this exclamation "!")
(this separator ", ")
(this lucky 7)

(this doubly lucky
  (+
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
        (the exclamation string)))))

(this hello world (greet "world"))

(this increment
  (with
    (any number)
    (do (+ (the number) 1))))

(this decrement
  (with
    (any number)
    (do (- (the number) 1))))

(this add
  (with
    (any number)
    (any number)
    (do
      (+ (the number number 1))
      (+ (the number number 2)))))
