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
    (normalize (stack) (normalized (variable 0)))
    (normalized (variable 0))))

(check
  (equal?
    (normalize (stack) (native "foo"))
    (normalized "foo")))

(check
  (equal?
    (normalize (stack) (type 12))
    (normalized (type 12))))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (native-application string-length
          (list (variable 0)))))
    (abstraction
      (native-application string-length
        (list (variable 0))))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (native-application string-length
            (list (variable 0))))
        (native "foo")))
    (normalized 3)))

(check
  (equal?
    (normalize (stack)
      (abstraction
        (abstraction
          (native-application string-append
            (list (variable 1) (variable 0))))))
    (abstraction
      (abstraction
        (native-application string-append
          (list (variable 1) (variable 0)))))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (abstraction
            (native-application string-append
              (list (variable 1) (variable 0)))))
        (native "foo")))
    (abstraction
      (native-application string-append
        (list (normalized "foo") (variable 0))))))

(check
  (equal?
    (normalize (stack)
      (application
        (application
          (abstraction
            (abstraction
              (native-application string-append
                (list (variable 1) (variable 0)))))
          (native "foo"))
        (native "bar")))
    (normalized "foobar")))

(check
  (equal?
    (normalize (stack)
      (abstraction-type
        (native number-type)
        (native boolean-type)))
    (abstraction-type
      (normalized number-type)
      (normalized boolean-type))))

(check
  (equal?
    (normalize (stack)
      (abstraction-type
        (type 0)
        (variable 0)))
    (abstraction-type
      (normalized (type 0))
      (variable 0))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction-type
          (native number-type)
          (native boolean-type))
        (native 10)))
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
        (native #t)
        (native "true")
        "false"))
    (normalized "true")))

(check
  (equal?
    (normalize (stack)
      (branch
        (native #f)
        "true"
        (native "false")))
    (normalized "false")))

(check
  (equal?
    (normalize (stack hole)
      (branch
        (variable 0)
        (native "true")
        (native "false")))
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

