(import (micascheme) (tico parser) (tico type) (tico term))

(check
  (equal?
    (syntax->typed #`(native fn (function number string (doing boolean))))
    (typed
      `fn
      (function-type
        (list (number-type) (string-type))
        (boolean-type)))))

(check
  (equal?
    (syntax->typed #`#f)
    (typed #f (boolean-type))))

(check
  (equal?
    (syntax->typed #`128)
    (typed 128 (number-type))))

(check
  (equal?
    (syntax->typed #`"foo")
    (typed "foo" (string-type))))

(check
  (equal?
    (syntax->typed #`foo)
    (typed
      (application `list (list))
      (struct-type `foo (list)))))

(check
  (equal?
    (syntax->typed #`(foo))
    (typed
      (application `list (list))
      (struct-type `foo (list)))))

(check
  (equal?
    (syntax->typed #`(foo #f 128 "foo"))
    (typed
      (application `list (list #f 128 "foo"))
      (struct-type `foo (list (boolean-type) (number-type) (string-type))))))

(check
  (equal?
    (syntax->typed #`(begin "foo"))
    (typed "foo" (string-type))))

(check
  (equal?
    (syntax->typed #`(begin (foo #f 128 "foo") (get string)))
    (typed
      (application `list-ref
        (list
          (application `list (list #f 128 "foo"))
          2))
      (string-type))))

(check
  (equal?
    (syntax->typed #`(function (doing 128)))
    (typed
      (function 0 128)
      (function-type (list) (number-type)))))

(check
  (equal?
    (syntax->typed #`(function boolean number string (doing 128)))
    (typed
      (function 3 128)
      (function-type
        (list (boolean-type) (number-type) (string-type))
        (number-type)))))

(check
  (equal?
    (syntax->typed #`(function boolean number string (doing (get boolean))))
    (typed
      (function 3 (variable 2))
      (function-type
        (list (boolean-type) (number-type) (string-type))
        (boolean-type)))))
