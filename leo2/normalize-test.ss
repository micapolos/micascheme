(import (leo2 base) (leo2 term) (leo2 normalize))

(data number-type)
(data string-type)

(check
  (equal?
    (normalize (stack) (normalized "foo"))
    (normalized "foo")))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (native string-length
          (list (variable 0)))))
    (abstraction
      (native string-length
        (list (variable 0))))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (native string-length
            (list (variable 0))))
        (normalized "foo")))
    (normalized 3)))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (abstraction
          (native string-append
            (list (variable 1) (variable 0))))))
    (abstraction
      (abstraction
        (native string-append
          (list (variable 1) (variable 0)))))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (abstraction
            (native string-append
              (list (variable 1) (variable 0)))))
        (normalized "foo")))
    (abstraction
      (native string-append
        (list (normalized "foo") (variable 0))))))

(check
  (equal?
    (normalize (stack)
      (application
        (application
          (abstraction
            (abstraction
              (native string-append
                (list (variable 1) (variable 0)))))
          (normalized "foo"))
        (normalized "bar")))
    (normalized "foobar")))
