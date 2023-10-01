(import (micascheme) (tico term) (tico type) (tico compiled))

(check
  (equal?
    (compiled-application
      (compiled
        (function-type
          (list (string-type) (string-type))
          (string-type))
        `string-append
        0
        (constant string-append))
      (list
        (string-compiled "foo")
        (string-compiled "bar")))
    (compiled
      (string-type)
      (application `string-append (list "foo" "bar"))
      0
      (constant "foobar"))))
