(import
  (micascheme)
  (tico reader)
  (tico typing)
  (tico type)
  (tico binding))

(check
  (equal?
    (read-typings (native "#f" "(+ 1 2)" "string-append"))
    (stack
      (native->typing #f)
      (native->typing '(+ 1 2))
      (native->typing 'string-append))))

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
    (read-typing string)
    (static-typing (value-type (string-type)))))

(check
  (equal?
    (read-typing (type boolean))
    (typing->type-typing (read-typing boolean))))

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
    (typing-promising
      (reverse (read-typings number string))
      (reverse (read-typings boolean)))))

(check
  (equal?
    (read-typing number (offering boolean))
    (typing-offering
      (read-typing number)
      (read-typing boolean))))

(check
  (equal?
    (read-typing foo (being number))
    (typing-being
      (read-typing foo)
      (read-typing number))))

(check
  (equal?
    (read-typings
      (prepare (native "(+ 1 2)")))
    (stack
      (type-datum->typing (unchecked-type) 3))))

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
    (read-typings 1 2 (the 3))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3))))

(check
  (equal?
    (read-typings 1 2 (with 3 (the 4)))
    (stack
      (literal->typing 1)
      (literal->typing 2)
      (literal->typing 3)
      (typing-struct 'the
        (list
          (literal->typing 4))))))

(check
  (equal?
    (read-typing
      (point (x 10) (y 20))
      (get x))
    (typing-ref
      (read-typing (point (x 10) (y 20)))
      (typing->type (read-typing x)))))

(check
  (equal?
    (read-typing
      (point (x 10) (y 20))
      (get x number))
    (typing-get
      (read-typing (point (x 10) (y 20)))
      (reverse (map typing->type (read-typings x number))))))

(check
  (equal?
    (read-typings
      (native "+")
      (as number number (promising number))
      (apply 3 4))
    (stack
      (typing-application
        (read-typing
          (native "+")
          (as number number (promising number)))
        (reverse (read-typings 3 4))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (read-typing
        string
        exclamate
        (doing (exclamated (get string)))
        (apply "foo" exclamate)))
    (with-generate-temporary-seed $tmp
      (lets
        ($params
          (list
            (generate-parameter-typing (string-type))
            (generate-parameter-typing (struct 'exclamate (list)))))
        (typing-application
          (typing-abstraction
            $params
            (list
              (typing-struct 'exclamated
                (list (variable-typing (string-type) '$tmp-0 0)))))
          (reverse (read-typings "foo" exclamate)))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (read-typing
        string
        (and number)
        (doing (get string))
        (apply "foo" (and 128))))
    (with-generate-temporary-seed $tmp
      (lets
        ($params
          (list
            (generate-parameter-typing (string-type))
            (generate-parameter-typing (struct 'and (list (number-type))))))
        (typing-application
          (typing-abstraction
            $params
            (list (variable-typing (string-type) '$tmp-0 1)))
          (reverse (read-typings "foo" (and 128))))))))

(check
  (equal?
    (read-typings 10 20
      (assert
        (native "(= 1 1)")
        (as boolean)))
    (read-typings 10 20)))

(check
  (raises?
    (lambda ()
      (read-typings 10 20
        (assert
          (native "(= 1 2)")
          (as boolean))))))

(check
  (equal?
    (read-typings "start" (load (tico (included foo bar))) "end")
    (stack
      (literal->typing "start")
      (literal->typing "foo")
      (literal->typing "bar")
      (literal->typing "end"))))

(check
  (equal?
    (typing-value
      (read-typing
        (use
          (native "string-append")
          (as
            string
            (plus string)
            (promising string)))
        "foo"
        (plus "bar")))
    "foobar"))

(check
  (equal?
    (typing-value
      (read-typing
        (use
          (native "string-length")
          (as string (offering (length number))))
        "foo"
        (get length number)))
    3))

(check
  (equal?
    (typing-value
      (read-typing
        (use
          (native "128")
          (as foo (being number)))
        foo))
    128))
