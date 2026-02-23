(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate))

(check-elaborates
  (type 12)
  (type 12))

(check-elaborate-throws
  (lambda (x) x))

(check-elaborates
  (application (lambda (x) x) (type 1))
  (typed
    (type 0)
    (application
      (typed
        (typed (type 0)
          (signature (type 2) (lambda (type 0))))
        (signature (type 2)
          (lambda
            (typed
              (typed (type 0) anything)
              (variable 0)))))
      (type 1))))

(check-elaborates
  (signature (type 0)
    (lambda (x) x))
  (typed
    (typed (type 0)
      (signature (type 0)
        (lambda (type 0))))
    (signature (type 0)
      (lambda
        (typed
          (typed (type 0) anything)
          (variable 0))))))

