(import
  (leo2 base)
  (leo2 term)
  (leo2 binding))

(check
  (binding?
    (binding "foo" (lambda (x) x))))

(check
  (not
    (binding?
      (application "foo" "bar"))))

(check
  (equal?
    (binding-ref
      (binding "foo"
        (lambda (x)
          (application (variable 'list) x))))
    "foo"))

(check
  (equal?
    (binding-apply
      (binding "foo"
        (lambda (x)
          (application (variable 'list) x)))
      (variable 'x))
    (application (variable 'list) (variable 'x))))
