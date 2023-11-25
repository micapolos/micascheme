(import
  (micascheme)
  (tico evaluation)
  (tico argument)
  (tico variable)
  (tico datum))

(check
  (equal?
    (evaluation-application
      (argument string-append)
      (list (argument "foo") (argument "bar")))
    (argument "foobar")))

(check
  (equal?
    (evaluation-application
      (variable 1)
      (list
        (argument "foo")
        (variable 3)))
    (variable 3)))
