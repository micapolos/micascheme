(import (leo2 base) (leo2 term) (leo2 normalize))

(data boolean-type)
(data number-type)
(data string-type)

; raises because of invalid term
(check
  (raises
    (normalize (stack) "error")))

(check
  (equal?
    (normalize (stack) (normalized "foo"))
    (normalized "foo")))

(check
  (equal?
    (normalize (stack) (type 12))
    (normalized (type 12))))

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

(check
  (equal?
    (normalize (stack)
      (abstraction-type
        (normalized number-type)
        (normalized boolean-type)))
    (abstraction-type
      (normalized number-type)
      (normalized boolean-type))))

(check
  (equal?
    (normalize (stack)
      (abstraction-type
        (normalized (type 0))
        (variable 0)))
    (abstraction-type
      (normalized (type 0))
      (variable 0))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction-type
          (normalized number-type)
          (normalized boolean-type))
        (normalized 10)))
    (normalized boolean-type)))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction-type
          (type 0)
          (variable 0))
        (normalized number-type)))
    (normalized number-type)))

(check
  (equal?
    (normalize (stack)
      (branch
        (normalized #t)
        (normalized "true")
        "false"))
    (normalized "true")))

(check
  (equal?
    (normalize (stack)
      (branch
        (normalized #f)
        "true"
        (normalized "false")))
    (normalized "false")))

(check
  (equal?
    (normalize (stack hole)
      (branch
        (variable 0)
        (normalized "true")
        (normalized "false")))
    (branch
      (variable 0)
      (normalized "true")
      (normalized "false"))))

; raises because branches are normalized and are invalid
(check
  (raises
    (normalize (stack hole)
      (branch
        (variable 0)
        "true"
        "false"))))

