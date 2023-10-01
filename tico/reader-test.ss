(import
  (micascheme)
  (leo reader)
  (tico reader)
  (tico term)
  (tico type))

(check
  (equal?
    (read-typed #f)
    (typed #f (boolean-type))))

(check
  (equal?
    (read-typed 128)
    (typed 128 (number-type))))

(check
  (equal?
    (read-typed "foo")
    (typed "foo" (string-type))))

(check
  (equal?
    (read-typed foo)
    (typed
      (application `list (list))
      (struct-type `foo (list)))))

(check
  (equal?
    (read-typed (tuple #f 128 "foo"))
    (typed
      (application `list (list #f 128 "foo"))
      (struct-type `tuple (list (boolean-type) (number-type) (string-type))))))

(check
  (equal?
    (read-typed (tuple #f (take 128 "foo")))
    (typed
      (application `list (list #f 128 "foo"))
      (struct-type `tuple (list (boolean-type) (number-type) (string-type))))))

(check
  (equal?
    (read-typed (tuple #f (do 128 "foo")))
    (typed
      (application `list (list 128 "foo"))
      (struct-type `tuple (list (number-type) (string-type))))))
