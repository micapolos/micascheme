(import
  (micascheme)
  (tico tico)
  (tico type))

(check
  (equal?
    (tico-item (quote 128))
    (typed
      (value-type 128)
      #f)))

(check
  (equal?
    (tico-item (quote (x 10 20)))
    (typed
      (value-type (struct-type 'x (list 10 20)))
      #f)))

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
