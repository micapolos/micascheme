(import (micascheme) (tico term) (tico type) (tico compiled))

(check
  (equal?
    (compiled-struct `foo
      (list
        (compiled-boolean #f)
        (compiled-number 128)
        (compiled-string "foo")))
    (compiled
      (struct-type `foo (list (boolean-type) (number-type) (string-type)))
      (application `list (list #f 128 "foo"))
      0
      (constant (list #f 128 "foo")))))

(check
  (equal?
    (compiled-struct `foo
      (list
        (compiled (boolean-type) `$boolean 3 #f)
        (compiled (number-type) `$number 5 #f)
        (compiled-string "foo")))
    (compiled
      (struct-type `foo (list (boolean-type) (number-type) (string-type)))
      (application `list (list `$boolean `$number "foo"))
      5
      #f)))

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
        (compiled-string "foo")
        (compiled-string "bar")))
    (compiled
      (string-type)
      (application `string-append (list "foo" "bar"))
      0
      (constant "foobar"))))
