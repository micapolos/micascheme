(import
  (micascheme)
  (tico type)
  (tico value)
  (tico expression)
  (tico parser))

(check
  (equal?
    (datum->compiled #f)
    (compiled
      (boolean-type)
      (combo (constant #f) #f))))

(check
  (equal?
    (datum->compiled 128)
    (compiled
      (number-type)
      (combo (constant 128) 128))))

(check
  (equal?
    (datum->compiled "foo")
    (compiled
      (string-type)
      (combo (constant "foo") "foo"))))

(check
  (equal?
    (datum->compiled `(vec 1 2))
    (compiled
      (struct-type `vec (list (number-type) (number-type)))
      (combo
        (constant (tuple-value (list 1 2)))
        (tuple-expression (list 1 2))))))

(check
  (raises?
    (lambda ()
      (datum->compiled `()))))
