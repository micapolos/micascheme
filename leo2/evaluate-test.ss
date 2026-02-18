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
    (lambda ($x) $x))
  (evaluated
    (abstraction-type
      (evaluated (type 0))
      (lambda (v0)
        (evaluated v0)))))

(check-evaluates
  (application
    (application
      (abstraction
        (lambda ($x)
          (abstraction
            (lambda ($y)
              (native-application string-append (list $x $y))))))
      (native "foo"))
    (native "bar"))
  (evaluated (native "foobar")))

(check-evaluates
  (branch (native #t) (native "true") 'error)
  (evaluated (native "true")))

(check-evaluates
  (branch (native #f) 'error (native "false"))
  (evaluated (native "false")))

(check-evaluates
  (branch (variable 'x) 'true-error 'false-error)
  (evaluated
    (branch
      (evaluated (variable 'x))
      'true-error
      'false-error)))

(check-evaluates
  (recursive
    (lambda ($self)
      (abstraction
        (lambda ($n) $self))))
  (evaluated
    (recursive
      (lambda (v0)
        (evaluated
          (abstraction
            (lambda (v1)
              (evaluated v0))))))))

(check-evaluates
  (recursive
    (lambda ($self)
      (abstraction
        (lambda ($n) $n))))
  (evaluated
    (recursive
      (lambda (v0)
        (evaluated
          (abstraction
            (lambda (v1)
              (evaluated v1))))))))

(check-evaluates
  (application
    (recursive
      (lambda ($self)
        (abstraction
          (lambda ($n)
            (branch
              (native-application zero? (list $n))
              (native "Done")
              (application $self (native-application - (list $n (native 1)))))))))
    (native 1))
  (evaluated (native "Done")))

(check-evaluates
  (application
    (recursive
      (lambda ($self)
        (abstraction
          (lambda ($n)
            (branch
              (native-application < (list $n (native 2)))
              $n
              (native-application +
                (list
                  (application $self (native-application - (list $n (native 1))))
                  (application $self (native-application - (list $n (native 2)))))))))))
    (native 10))
  (evaluated (native 55)))
