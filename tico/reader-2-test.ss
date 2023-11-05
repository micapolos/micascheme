(import
  (micascheme)
  (tico reader-2)
  (tico typing))

(check
  (equal?
    (read-typings (native "1" "(+ 1 2)" "+"))
    (stack
      (native->typing 1)
      (native->typing '(+ 1 2))
      (native->typing '+))))

(check
  (equal?
    (read-typing foo)
    (static-typing (struct 'foo (list)))))

(check
  (equal?
    (read-typing "foo")
    (literal->typing "foo")))

(check
  (equal?
    (read-typing (x 128 "foo"))
    (typing-struct 'x
      (list
        (literal->typing 128)
        (literal->typing "foo")))))

(check
  (equal?
    (read-typings 1 2 (take 3 4))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3)
      (literal->typing 4))))

(check
  (equal?
    (read-typings 1 2 (with 3 (take 4 5)))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3)
      (typing-struct 'take
        (list
          (literal->typing 4)
          (literal->typing 5))))))

(check
  (equal?
    (read-typings 1 2 (do 3 4))
    (stack
      (literal->typing 3)
      (literal->typing 4))))
