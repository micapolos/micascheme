(import
  (micascheme)
  (tico reader)
  (tico typing)
  (tico type)
  (tico binding))

(check
  (equal?
    (read-typing (native "(+ 1 2)"))
    (native->typing '(+ 1 2))))

(check
  (equal?
    (read-typings 
      10 
      (comment (this is a comment) 10 "foo")
      20)
    (stack
      (read-typing 10)
      (read-typing 20))))

(check
  (equal?
    (read-typings boolean number string)
    (stack
      (static-typing (value-type (boolean-type)))
      (static-typing (value-type (number-type)))
      (static-typing (value-type (string-type))))))

(check
  (equal?
    (read-typings 10 (type boolean number string))
    (stack
      (read-typing 10)
      (typing->type-typing (read-typing boolean))
      (typing->type-typing (read-typing number))
      (typing->type-typing (read-typing string)))))

(check
  (equal?
    (read-typings
      (native "(+ 1 2)")
      (as number))
    (stack
      (type-datum->typing
        (number-type)
        '(+ 1 2)))))

(check
  (equal?
    (read-typing
      number
      string
      (promising boolean))
    (typings-promising
      (read-typings number string)
      (read-typing boolean))))

(check
  (equal?
    (read-typings number string (offering boolean))
    (typings-offering
      (read-typings number string)
      (read-typings boolean))))

(check
  (equal?
    (read-typings
      (prepare
        (native "(+ 1 2)")
        (native "(string-append \"foo\" \"bar\")")))
    (stack
      (type-datum->typing (native-type) 3)
      (type-datum->typing (native-type) "foobar"))))

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
    (read-typings 1 2 (the 3 4))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3)
      (literal->typing 4))))

(check
  (equal?
    (read-typings 1 2 (with 3 (the 4 5)))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3)
      (typing-struct 'the
        (list
          (literal->typing 4)
          (literal->typing 5))))))

(check
  (equal?
    (read-typing
      (point (x 10) (y 20))
      (get x number))
    (typings-get
      (read-typings (point (x 10) (y 20)))
      (read-typings x number))))

(check
  (equal?
    (with-tmps
      (read-typing
        128 "foo"
        (do (get string))))
    (with-tmps
      (bindings-do (stack)
        (reverse (read-typings 128 "foo"))
        (lambda ($bindings)
          (bindings-read-typing $bindings
            (get string)))))))

(check
  (equal?
    (read-typings
      (the
        (native "+")
        (as number number (promising number)))
      (the
        (native "*")
        (as number number (promising number)))
      (apply 3 4))
    (stack
      (typing-application
        (read-typing
          (the
            (native "+")
            (as number number (promising number))))
        (list
          (read-typing 3)
          (read-typing 4)))
      (typing-application
        (read-typing
          (the
            (native "*")
            (as number number (promising number))))
        (list
          (read-typing 3)
          (read-typing 4))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (read-typing
        string number
        (doing (get string))
        (apply "foo" 128)))
    (with-generate-temporary-seed $tmp
      (lets
        ($params
          (list
            (generate-parameter-typing (string-type))
            (generate-parameter-typing (number-type))))
        (typing-application
          (typing-abstraction $params
            (variable-typing (string-type) '$tmp-0 1))
          (reverse (read-typings "foo" 128)))))))

(check
  (equal?
    (read-typings
      "foo" "bar"
      (assert (native "(= 1 1)") (as boolean)))
    (read-typings "foo" "bar")))

(check
  (equal?
    (read-typings "start" (load (tico (included foo bar))) "end")
    (stack
      (literal->typing "start")
      (literal->typing "foo")
      (literal->typing "bar")
      (literal->typing "end"))))

