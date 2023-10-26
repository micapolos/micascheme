(import
  (micascheme)
  (tico type)
  (tico value)
  (tico expression)
  (tico parser))

(check
  (equal?
    (datum->compiled `boolean)
    (compiled (value-type (boolean-type)) #f)))

(check
  (equal?
    (datum->compiled `number)
    (compiled (value-type (number-type)) #f)))

(check
  (equal?
    (datum->compiled `string)
    (compiled (value-type (string-type)) #f)))

(check
  (equal?
    (datum->compiled `(scheme number n))
    (compiled (number-type) (expr `n))))

(check
  (equal?
    (datum->compiled #f)
    (compiled (value-type #f) #f)))

(check
  (equal?
    (datum->compiled 128)
    (compiled (value-type 128) #f)))

(check
  (equal?
    (datum->compiled "foo")
    (compiled (value-type "foo") #f)))

(check
  (equal?
    (datum->compiled `(vec (scheme number n) (scheme string s)))
    (compiled
      (struct-type `vec (list (number-type) (string-type)))
      (expr (tuple-expression (list `n `s))))))

(check
  (equal?
    (datum->compiled `(type (scheme number 1)))
    (compiled (value-type (number-type)) #f)))

(check
  (raises?
    (lambda ()
      (datum->compiled `()))))