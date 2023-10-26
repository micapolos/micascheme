(import
  (micascheme)
  (tico type)
  (tico value)
  (tico expression)
  (tico parser))

(check
  (equal?
    (datum->compiled `boolean)
    (value-compiled (boolean-type))))

(check
  (equal?
    (datum->compiled `number)
    (value-compiled (number-type))))

(check
  (equal?
    (datum->compiled `string)
    (value-compiled (string-type))))

(check
  (equal?
    (datum->compiled `(scheme number n))
    (compiled (number-type) (expr `n))))

(check
  (equal?
    (datum->compiled #f)
    (value-compiled #f)))

(check
  (equal?
    (datum->compiled 128)
    (value-compiled 128)))

(check
  (equal?
    (datum->compiled "foo")
    (value-compiled "foo")))

(check
  (equal?
    (datum->compiled `(vec (scheme number n) (scheme string s)))
    (compiled
      (struct-type `vec (list (number-type) (string-type)))
      (expr (tuple-expression (list `n `s))))))

(check
  (equal?
    (datum->compiled `(type (scheme number 1)))
    (value-compiled (number-type))))

(check
  (raises?
    (lambda ()
      (datum->compiled `()))))
