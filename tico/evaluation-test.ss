(import
  (micascheme)
  (tico evaluation)
  (tico constant)
  (tico variable)
  (tico datum))

(check
  (equal?
    (evaluation-application
      (constant string-append)
      (list (constant "foo") (constant "bar")))
    (constant "foobar")))

(check
  (equal?
    (evaluation-application
      (variable 1)
      (list
        (constant "foo")
        (variable 3)))
    (variable 3)))
