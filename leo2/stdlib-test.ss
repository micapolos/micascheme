(import
  (leo2 base)
  (leo2 stdlib)
  (leo2 equal)
  (leo2 term))

(check
  (binding?
    (binding
      (native "foo")
      (abstraction (lambda (x) x)))))

(check
  (not
    (binding?
      (application (variable 'x) (native "bar")))))

(check
  (equal?
    (binding-ref
      (binding
        (native "foo")
        (lambda (x)
          (application (variable 'list) x))))
    (native "foo")))

(check
  (equal?
    (binding-apply
      (binding
        (native "foo")
        (lambda (x)
          (application (variable 'list) x)))
      (variable 'x))
    (application (variable 'list) (variable 'x))))

(check-term=?
  (type-of
    (abstraction-term string-type
      (lambda (x)
        (abstraction-term string-type
          (lambda (y)
            (native-application-term string-type string-append x y))))))
  (abstraction-type-term string-type
    (lambda (_)
      (abstraction-type-term string-type
        (lambda (_)
          string-type)))))

(check-term=?
  (type-of
    (application-term
      (application-term
        (abstraction-term string-type
          (lambda (x)
            (abstraction-term string-type
              (lambda (y)
                (native-application-term string-type string-append x y)))))
        (string-term "foo"))
      (string-term "bar")))
  string-type)

(check-term=?
  (type-of
    (branch-term
      (variable-term boolean-type 'a)
      (variable-term string-type 'b)
      (variable-term number-type 'c)))
  (branch-type-term
    (variable-term boolean-type 'a)
    string-type
    number-type))
