(import (leo2 base) (leo2 term) (leo2 normalize))

(data boolean-type)
(data number-type)
(data string-type)

(define native-string-length
  (abstraction
    (native-application string-length (list (variable 0)))))

(define native-string-append
  (abstraction
    (abstraction
      (native-application string-append (list (variable 1) (variable 0))))))

(define native+
  (abstraction
    (abstraction
      (native-application + (list (variable 1) (variable 0))))))

(define native-
  (abstraction
    (abstraction
      (native-application - (list (variable 1) (variable 0))))))

(define native<
  (abstraction
    (abstraction
      (native-application < (list (variable 1) (variable 0))))))

; raises because of invalid term
(check
  (raises
    (normalize (stack) 'error)))

(check
  (equal?
    (normalize (stack) (normalized (variable 0)))
    (normalized (variable 0))))

(check
  (equal?
    (normalize (stack) (native "foo"))
    (normalized (native "foo"))))

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
    (normalized (native 3))))

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
        (list
          (normalized (native "foo"))
          (variable 0))))))

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
    (normalized (native "foobar"))))

(check
  (equal?
    (normalize (stack)
      (abstraction-type
        (native number-type)
        (native boolean-type)))
    (abstraction-type
      (normalized (native number-type))
      (normalized (native boolean-type)))))

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
    (normalized (native boolean-type))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction-type
          (type 0)
          (variable 0))
        (native number-type)))
    (normalized
      (native number-type))))

(check
  (equal?
    (normalize (stack)
      (branch
        (native #t)
        (native "true")
        "false"))
    (normalized (native "true"))))

(check
  (equal?
    (normalize (stack)
      (branch
        (native #f)
        "true"
        (native "false")))
    (normalized (native "false"))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (branch
            (variable 0)
            (native "true")
            (native "false")))
        (native #t)))
    (normalized (native "true"))))

(check
  (equal?
    (normalize (stack)
      (application
        (abstraction
          (branch
            (variable 0)
            (native "true")
            (native "false")))
        (native #f)))
    (normalized (native "false"))))

(check
  (equal?
    (normalize (stack hole)
      (branch
        (variable 0)
        (native "true")
        (native "false")))
    (branch
      (variable 0)
      (normalized (native "true"))
      (normalized (native "false")))))

; raises because branches are normalized and are invalid
(check
  (raises
    (normalize (stack hole)
      (branch
        (variable 0)
        'true-error
        'false-error))))

(check
  (equal?
    (normalize (stack)
      (recursive
        (abstraction
          (native "foo"))))
    (recursive
      (abstraction
        (normalized (native "foo"))))))

(check
  (equal?
    (normalize (stack)
      (application
        (recursive
          (abstraction
            (variable 1)))
        (native "foo")))
    (recursive
      (abstraction
        (variable 1)))))

(check
  (equal?
    (normalize (stack)
      (application
        (recursive
          (abstraction
            (variable 0)))
        (native "foo")))
    (normalized (native "foo"))))

; fib

; (check
;   (equal?
;     (normalize (stack)
;       (application
;         (abstraction
;           (branch
;             (application (application native< (variable 0)) (native 2))
;             (native 10)
;             (native 20)))
;         (native 0)))
;     (normalized 123)))

