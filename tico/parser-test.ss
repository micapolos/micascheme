(import (micascheme) (tico parser) (tico type) (tico term))

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
    (syntax->typed #`(begin "foo"))
    (typed "foo" (string-type))))

(check
  (equal?
    (syntax->typed #`(foo #f 128 "foo"))
    (typed
      (application `list (list #f 128 "foo"))
      (struct-type `foo (list (boolean-type) (number-type) (string-type))))))
