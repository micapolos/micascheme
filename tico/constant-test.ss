(import
  (micascheme)
  (tico constant)
  (tico datum)
  (tico value))

(check (equal? (constant-arity (constant)) (value-arity (list))))
(check (equal? (constant-arity (constant 1 2 3)) (arity 3)))

(check
  (equal?
    (datum->constant '(string-append "foo" "bar"))
    (constant (datum->value '(string-append "foo" "bar")))))

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
