(import
  (micascheme)
  (tico arity)
  (tico evaluation)
  (tico constant)
  (tico variable)
  (tico parameter)
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

(check
  (equal?
    (evaluation-parameters
      (arity 2)
      (constant "foo" "bar"))
    (constant-parameters
      (constant "foo" "bar"))))

(check
  (equal?
    (evaluation-parameters
      (arity 2)
      (variable 128))
    (list
      (parameter)
      (parameter))))

(check
  (equal?
    (evaluation-parameters
      (arity 2)
      (parameter))
    (list
      (parameter)
      (parameter))))
