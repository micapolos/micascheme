(import
  (micascheme)
  (tico constant)
  (tico datum))

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
