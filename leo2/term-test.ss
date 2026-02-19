(import (leo2 base) (leo2 term))

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
