(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate))

(check-elaborates
  (type 12)
  (type 12))

(check-elaborate-throws (native "foo"))

(check-elaborates
  (typed (type 2) (native "foo"))
  (typed (type 2) (native "foo")))

(check-elaborate-throws
  (lambda (x) x))

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
