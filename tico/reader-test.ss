(import
  (micascheme)
  (leo reader)
  (tico reader)
  (tico term)
  (tico type)
  (tico compiled))

(check
  (equal?
    (read-compiled #f)
    (compiled-boolean #f)))

(check
  (equal?
    (read-compiled 128)
    (compiled-number 128)))

(check
  (equal?
    (read-compiled "foo")
    (compiled-string "foo")))

(check
  (equal?
    (read-compiled (tuple #f 128 "foo"))
    (compiled-struct `tuple
      (list
        (compiled-boolean #f)
        (compiled-number 128)
        (compiled-string "foo")))))

(check
  (equal?
    (read-compiled (tuple #f (take 128 "foo")))
    (compiled-struct `tuple
      (list
        (compiled-boolean #f)
        (compiled-number 128)
        (compiled-string "foo")))))

(check
  (equal?
    (read-compiled (tuple #f (do 128 "foo")))
    (compiled-struct `tuple
      (list
        (compiled-number 128)
        (compiled-string "foo")))))
