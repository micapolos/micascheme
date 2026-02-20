(import
  (leo2 base)
  (leo2 term)
  (leo2 equal))

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
