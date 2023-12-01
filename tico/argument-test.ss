(import
  (micascheme)
  (tico argument)
  (tico datum)
  (tico value)
  (tico arity)
  (tico index)
  (tico tuple))

; --- argument-arity

(check (equal? (argument-arity (argument)) (value-arity (list))))
(check (equal? (argument-arity (argument 1 2 3)) (arity 3)))

; --- arguments-values

(check
  (equal?
    (arguments-values
      (list
        (argument)
        (argument 1)
        (argument 2 3)))
    (list 1 2 3)))

; --- datum->argument

(check
  (equal?
    (datum->argument '(string-append "foo" "bar"))
    (argument (datum->value '(string-append "foo" "bar")))))

(check
  (equal?
    (datum->argument '(values))
    (argument)))

(check
  (equal?
    (datum->argument '(values 1))
    (argument 1)))

(check
  (equal?
    (datum->argument '(values 1 2))
    (argument 1 2)))

(check
  (equal?
    (bindings-datum->argument
      (stack
        (cons 'foo "foo")
        (cons 'bar "bar"))
      '(string-append "foo" "bar"))
    (argument
      (bindings-datum->value
        (stack
          (cons 'foo "foo")
          (cons 'bar "bar"))
        '(string-append "foo" "bar")))))

; --- argument-application

(check
  (equal?
    (argument-application
      (argument-abstraction 2 (list (argument "foo")))
      (list (argument 10) (argument 20)))
    (argument "foo")))

(check
  (equal?
    (argument-application
      (argument string-append)
      (list (argument "foo") (argument "bar")))
    (argument "foobar")))

(check
  (equal?
    (argument-application
      (argument string-append)
      (list
        (argument)
        (argument "foo")
        (argument "bar" "gar")))
    (argument "foobargar")))

(check
  (raises?
    (lambda ()
      (argument-application
        (argument)
        (list)))))

; --- argument-abstraction

(check
  (equal?
    (argument-application
      (argument-abstraction 2
        (list (argument "res")))
      (list
        (argument "arg1")
        (argument "arg2")))
    (argument "res")))

(check
  (equal?
    (argument-application
      (argument-abstraction 2
        (list
          (argument)
          (argument "res1")
          (argument "res2" "res3")))
      (list
        (argument "arg1")
        (argument "arg2")))
    (argument "res1" "res2" "res3")))

; --- argument-tuple

(check
  (equal?
    (argument-tuple
      (list
        (argument)
        (argument "foo")
        (argument 128 #f)))
    (argument
      (tuple "foo" 128 #f))))

; --- argument-tuple-ref

(check
  (equal?
    (argument-tuple-ref
      (arity 3)
      (argument-tuple
        (list
          (argument)
          (argument "foo")
          (argument 128 #f)))
      (index 1))
    (argument
      (tuple-ref 3 (tuple "foo" 128 #f) 1))))
