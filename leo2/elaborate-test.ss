(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate)
  (leo2 stdlib))

; --- typed

(check-elaborates
  (typed (type 2) (native "foo"))
  (typed (type 2) (native "foo")))

; --- type

(check-elaborates
  (type 12)
  (type 12))

; --- native

(check-elaborate-throws
  (native "foo"))

(check-elaborate-throws
  (application
    (signature (type 1) (lambda (x) x))
    (native "not-a-type-0")))

; --- evaluated

(check-elaborates
  (evaluated (type 0))
  (type 0))

; --- native-application

(check-elaborates
  (native-application zero? (list (variable 100)))
  (typed
    (typed (type 0) anything)
    (native-application ,zero?
      (list
        (typed
          (typed (type 0) anything)
          (variable 100))))))

; --- variable

(check-elaborates
  (variable 100)
  (typed
    (typed (type 0) anything)
    (variable 100)))

; --- procedure

(check-elaborate-throws
  (lambda (x) x))

; --- signature

(check-elaborates
  (signature (type 0)
    (lambda (x) x))
  (typed
    (typed
      (type 0)
      (signature
        (type 0)
        (lambda (type 0))))
    (signature (type 0)
      (lambda
        (typed
          (type 0)
          (variable 0))))))

; --- application

(check-elaborates
  (application
    (lambda (x) x)
    (type 50))
  (typed
    (type 51)
    (application
      (typed
        (typed
          (type 0)
          (signature
            (type 51)
            (lambda (type 51))))
        (signature (type 51)
          (lambda
            (typed
              (type 51)
              (variable 0)))))
      (type 50))))

; --- branch

; Case 1: Matching types
(check-elaborates
  (branch
    (typed (type 0) (native boolean-type))
    (type 0)
    (type 0))
  (typed
    (branch
      (native ,boolean-type)
      (type 1)
      (type 1))
    (branch
      (typed (type 0) (native ,boolean-type))
      (type 0)
      (type 0))))

; Case 2: Mismatching types (Dependent Type Selection)
(check-elaborates
  (branch
    (typed (type 0) (native boolean-type))
    (type 0)
    (type 1))
  (typed
    (branch
      (native ,boolean-type)
      (type 1)
      (type 2))
    (branch
      (typed (type 0) (native ,boolean-type))
      (type 0)
      (type 1))))

; Case 3: Neutral condition
(check-elaborates
  (branch
    (variable 100)
    (type 0)
    (type 0))
  (typed
    (branch
      (variable 100)
      (type 1)
      (type 1))
    (branch
      (typed (typed (type 0) anything) (variable 100))
      (type 0)
      (type 0))))

; --- recursion

(check-elaborates
  (recursion
    (signature (type 0)
      (lambda (x) x)))
  (typed
    (typed
      (type 0)
      (signature (type 0) (lambda (type 0))))
    (recursion
      (signature (type 0)
        (lambda
          (typed
            (type 0)
            (variable 0)))))))

; --- labeled

(check-elaborates
  (labeled (variable 100) (type 0))
  (typed
    (type 1)
    (labeled (variable 100) (type 0))))
