(import
  (micascheme)
  (tico constant)
  (tico datum)
  (tico value))

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

(check
  (equal?
    (constant-application
      (constant-abstraction 2 (constant "foo"))
      (list (constant 10) (constant 20)))
    (constant "foo")))
