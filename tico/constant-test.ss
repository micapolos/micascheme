(import
  (micascheme)
  (tico constant)
  (tico datum)
  (tico value)
  (tico arity)
  (tico index)
  (tico tuple))

; --- constant-arity

(check (equal? (constant-arity (constant)) (value-arity (list))))
(check (equal? (constant-arity (constant 1 2 3)) (arity 3)))

; --- constants-values

(check
  (equal?
    (constants-values
      (list
        (constant)
        (constant 1)
        (constant 2 3)))
    (list 1 2 3)))

; --- datum->constant

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
      (constant-abstraction 2 (list (constant "foo")))
      (list (constant 10) (constant 20)))
    (constant "foo")))

(check
  (equal?
    (constant-application
      (constant string-append)
      (list (constant "foo") (constant "bar")))
    (constant "foobar")))

(check
  (equal?
    (constant-application
      (constant string-append)
      (list
        (constant)
        (constant "foo")
        (constant "bar" "gar")))
    (constant "foobargar")))

(check
  (raises
    (constant-application
      (constant)
      (list))))

; --- constant-abstraction

(check
  (equal?
    (constant-application
      (constant-abstraction 2
        (list (constant "res")))
      (list
        (constant "arg1")
        (constant "arg2")))
    (constant "res")))

(check
  (equal?
    (constant-application
      (constant-abstraction 2
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

; --- constant-parameters

(check (equal? (constant-parameters (constant)) (list)))
(check (equal? (constant-parameters (constant "foo")) (list (constant "foo"))))
(check (equal? (constant-parameters (constant "foo" "bar")) (list (constant "foo") (constant "bar"))))
