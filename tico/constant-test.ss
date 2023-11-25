(import
  (micascheme)
  (tico constant)
  (tico datum)
  (tico value)
  (tico arity)
  (tico index)
  (tico tuple))

(check (equal? (constant-arity (constant)) (value-arity (list))))
(check (equal? (constant-arity (constant 1 2 3)) (arity 3)))

(check
  (equal?
    (constants-values
      (list
        (constant)
        (constant 1)
        (constant 2 3)))
    (list 1 2 3)))

(check
  (equal?
    (datum->constant '(string-append "foo" "bar"))
    (constant (datum->value '(string-append "foo" "bar")))))

(check
  (equal?
    (datum->constant '(values))
    (constant)))

(check
  (equal?
    (datum->constant '(values 1))
    (constant 1)))

(check
  (equal?
    (datum->constant '(values 1 2))
    (constant 1 2)))

(check
  (equal?
    (bindings-datum->constant
      (stack
        (cons 'foo "foo")
        (cons 'bar "bar"))
      '(string-append "foo" "bar"))
    (constant
      (bindings-datum->value
        (stack
          (cons 'foo "foo")
          (cons 'bar "bar"))
        '(string-append "foo" "bar")))))

; --- constant-application

(check
  (equal?
    (constant-application
      (constant-abstraction 2 (constant "foo"))
      (list (constant 10) (constant 20)))
    (constant "foo")))

(check
  (equal?
    (constant-application-2
      (constant string-append)
      (list (constant "foo") (constant "bar")))
    (constant "foobar")))

(check
  (equal?
    (constant-application-2
      (constant string-append "foo")
      (list
        (constant)
        (constant "bar")
        (constant "goo" "zoo")))
    (constant "foobargoozoo")))

(check
  (raises?
    (lambda ()
      (constant-application-2
        (constant)
        (list)))))

; --- constant-abstraction

(check
  (equal?
    (constant-application
      (constant-abstraction-2 2
        (list (constant "res")))
      (list
        (constant "arg1")
        (constant "arg2")))
    (constant "res")))

(check
  (equal?
    (constant-application-2
      (constant-abstraction-2 2
        (list
          (constant)
          (constant "res1")
          (constant "res2" "res3")))
      (list
        (constant "arg1")
        (constant "arg2")))
    (constant "res1" "res2" "res3")))

; --- constant-tuple

(check
  (equal?
    (constant-tuple
      (list
        (constant)
        (constant "foo")
        (constant 128 #f)))
    (constant
      (tuple "foo" 128 #f))))

; --- constant-tuple-ref

(check
  (equal?
    (constant-tuple-ref
      (arity 3)
      (constant-tuple
        (list
          (constant)
          (constant "foo")
          (constant 128 #f)))
      (index 1))
    (constant
      (tuple-ref 3 (tuple "foo" 128 #f) 1))))
