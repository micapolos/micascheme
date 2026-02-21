(import
  (leo2 base)
  (leo2 term)
  (leo2 equal)
  (leo2 stdlib))

(check-term=? (type-of (type 0)) (type 1))
(check-term=? (type-of (type 1)) (type 2))

(check-term=?
  (type-of
    (typed
      (typed (type 0) (native 'a-boolean))
      (native #t)))
  (typed (type 0) (native 'a-boolean)))

(check
  (binding?
    (binding
      (string-term "foo")
      (abstraction 't (lambda (x) x)))))

(check
  (not
    (binding?
      (application (variable 'x)
        (string-term "foo")))))

(check
  (equal?
    (binding-ref
      (binding
        (string-term "foo")
        (lambda (x)
          (application (variable 'list) x))))
    (string-term "foo")))

(check
  (equal?
    (binding-apply
      (binding
        (string-term "foo")
        (lambda (x)
          (application (variable 'list) x)))
      (variable 'x))
    (application (variable 'list) (variable 'x))))
