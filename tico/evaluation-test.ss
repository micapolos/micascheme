(import
  (micascheme)
  (tico evaluation)
  (tico constant)
  (tico dependency)
  (tico variable)
  (tico packet)
  (tico datum))

(check
  (equal?
    (evaluation-application
      (constant string-append)
      (list (constant "foo") (constant "bar"))
      (lambda () (throw error)))
    (constant "foobar")))

(check
  (equal?
    (evaluation-lets-datums
      (variable 1 (stack (test-dependency dep))))
    (variable-lets-datums
      (variable 1 (stack (test-dependency dep))))))

(check
  (equal?
    (evaluation-lets-datums
      (constant "foo"))
    (list)))

(check
  (equal?
    (evaluation-value (constant "foo"))
    "foo"))

(check
  (raises?
    (lambda ()
      (evaluation-value (variable 1 (stack))))))

(check
  (equal?
    (evaluation-application
      (variable 1
        (stack
          (test-dependency d1)
          (test-dependency d2)))
      (list
        (constant "foo")
        (variable 3
          (stack
            (test-dependency d3)
            (test-dependency d4))))
      (lambda ()
        (stack
          (test-dependency c1)
          (test-dependency c2))))
    (variable 3
      (stack
        (test-dependency c1)
        (test-dependency c2)
        (test-dependency d1)
        (test-dependency d2)
        (test-dependency d3)
        (test-dependency d4)))))

(check
  (equal?
    (evaluation-abstraction
      2
      (constant "foo")
      (lambda () (throw error)))
    (constant "foo")))

(check
  (equal?
    (evaluation-abstraction
      2
      (variable 3
        (stack
          (dependency 'v1 (packet "foo" "foo"))
          (dependency 'v2 (packet "bar" "bar"))))
      (lambda () (throw error)))
    (variable 1
      (stack
        (dependency 'v1 (packet "foo" "foo"))
        (dependency 'v2 (packet "bar" "bar"))))))

(check
  (equal?
    (evaluation-abstraction
      2
      (variable 1
        (stack
          (dependency 'v1 (packet "foo" "foo"))
          (dependency 'v2 (packet "bar" "bar"))))
      (lambda () '(string-append "foo" "bar")))
    (constant "foobar")))
