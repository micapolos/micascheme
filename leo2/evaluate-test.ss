(import
  (leo2 base)
  (leo2 term)
  (leo2 evaluate)
  (leo2 datum))

(check-evaluates (hole 12) (evaluated (hole 12)))

(check-evaluates nothing (evaluated nothing))

(check-evaluates (type 12) (evaluated (type 12)))

(check-evaluates (variable 0) (evaluated (variable 0)))

(check-evaluates
  (native string-append)
  (evaluated (native string-append)))

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
      (variable 0)))
  (evaluated
    (native-application string-append
      (list
        (evaluated (native "foo"))
        (evaluated (variable 0))))))

(check-evaluates
  (lambda (x) x)
  (evaluated (lambda (x) (evaluated x))))

(check-evaluates
  (application
    (lambda (x) x)
    (native "foo"))
  (evaluated (native "foo")))

(check-evaluates
  (signature
    (type 0)
    (lambda (x) x))
  (evaluated
    (signature
      (evaluated (type 0))
      (lambda (v0) (evaluated v0)))))

(check-evaluates
  (application
    (application
      (lambda (x)
        (lambda (y)
          (native-application string-append (list x y))))
      (native "foo"))
    (native "bar"))
  (evaluated (native "foobar")))

(check-evaluates
  (branch
    (native #t)
    (native "true")
    (native "false"))
  (evaluated (native "true")))

(check-evaluates
  (branch
    (native #f)
    (native "true")
    (native "false"))
  (evaluated (native "false")))

(check-evaluates
  (branch
    (variable 0)
    (variable 1)
    (variable 1))
  (evaluated (variable 1)))

(check-evaluates
  (branch
    (variable 0)
    (variable 1)
    (variable 2))
  (evaluated
    (branch
      (evaluated (variable 0))
      (evaluated (variable 1))
      (evaluated (variable 2)))))

(check-evaluates
  (recursion
    (lambda (fn)
      (lambda (n) fn)))
  (evaluated
    (recursion
      (lambda (v0)
        (evaluated
          (lambda (v1)
            (evaluated v0)))))))

(check-evaluates
  (recursion
    (lambda (fn)
      (lambda (n) n)))
  (evaluated
    (recursion
      (lambda (v0)
        (evaluated
          (lambda (v1)
            (evaluated v1)))))))

(check-evaluates
  (application
    (recursion
      (lambda (fn)
        (lambda (n)
          (branch
            (native-application zero? (list n))
            (native "Done")
            (application fn (native-application - (list n (native 1))))))))
    (native 10))
  (evaluated (native "Done")))

(check-evaluates
  (application
    (recursion
      (lambda (fib)
        (lambda (n)
          (branch
            (native-application < (list n (native 2)))
            n
            (native-application +
              (list
                (application fib (native-application - (list n (native 1))))
                (application fib (native-application - (list n (native 2))))))))))
    (native 10))
  (evaluated (native 55)))

(check-evaluates
  (typed
    (native "foo")
    (native "bar"))
  (evaluated
    (typed
      (native "foo")
      (evaluated (native "bar")))))
