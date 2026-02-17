(import
  (leo2 base)
  (leo2 term)
  (leo2 evaluate)
  (leo2 datum))

(check-evaluates
  (evaluated (native "foo"))
  (evaluated (native "foo")))

(check-evaluates
  (type 12)
  (evaluated (type 12)))

(check-evaluates
  (variable 'x)
  (evaluated (variable 'x)))

(check-evaluates
  (native "foo")
  (evaluated (native "foo")))

(check-evaluates
  (native-application string-append
    (list
      (native "foo")
      (native "bar")))
  (evaluated (native "foobar")))

(check-evaluates
  (native-application string-append
    (list
      (native "foo")
      (variable 'x)))
  (evaluated
    (native-application string-append
      (list
        (evaluated (native "foo"))
        (evaluated (variable 'x))))))

(check-evaluates
  (native-application string-append
    (list
      (native "foo")
      (variable 'x)))
  (evaluated
    (native-application string-append
      (list
        (evaluated (native "foo"))
        (evaluated (variable 'x))))))

(check-evaluates
  (abstraction (lambda ($x) $x))
  (evaluated
    (abstraction
      (lambda (v0)
        (evaluated v0)))))

(check-evaluates
  (application
    (abstraction (lambda ($x) $x))
    (native "foo"))
  (evaluated
    (native "foo")))

(check-evaluates
  (abstraction-type
    (type 0)
    (abstraction
      (lambda ($x) $x)))
  (evaluated
    (abstraction-type
      (evaluated (type 0))
      (abstraction
        (lambda (v0)
          (evaluated v0))))))

(check-evaluates
  (branch (native #t) (native "true") 'error)
  (evaluated (native "true")))

(check-evaluates
  (branch (native #f) 'error (native "false"))
  (evaluated (native "false")))

(check-evaluates
  (branch (variable 'x) (native "true") (native "false"))
  (evaluated
    (branch
      (evaluated (variable 'x))
      (evaluated (native "true"))
      (evaluated (native "false")))))
