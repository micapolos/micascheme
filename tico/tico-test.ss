(import
  (micascheme)
  (tico tico)
  (tico type))

(check
  (equal?
    (tico-item #f)
    (typed
      (boolean-type)
      (phased
        #f
        (constant #f)))))

(check
  (equal?
    (tico-item 128)
    (typed
      (number-type)
      (phased
        128
        (constant 128)))))

(check
  (equal?
    (tico-item "foo")
    (typed
      (string-type)
      (phased
        "foo"
        (constant "foo")))))
