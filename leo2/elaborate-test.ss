(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate))

;; --- Basic Types & Natives ---

(check-elaborates
  (type 12)
  (type 12))

(check-elaborate-throws (native "foo"))

(check-elaborates
  (typed (type 2) (native "foo"))
  (typed (type 2) (native "foo")))

;; --- Procedures & Signatures ---

(check-elaborate-throws
  (lambda (x) x))

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

;; --- Applications ---

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

;; --- Dependent Branches ---

;; Case 1: Matching types
(check-elaborates
  (branch
    (typed (type 0) (native 'boolean))
    (type 0)
    (type 0))
  (typed
    (branch
      (native boolean)
      (type 1)
      (type 1))
    (branch
      (typed (type 0) (native boolean))
      (type 0)
      (type 0))))

;; Case 2: Mismatching types (Dependent Type Selection)
(check-elaborates
  (branch
    (typed (type 0) (native 'boolean))
    (type 0)
    (type 1))
  (typed
    (branch
      (native boolean)
      (type 1)
      (type 2))
    (branch
      (typed (type 0) (native boolean))
      (type 0)
      (type 1))))

;; --- Recursion & Labeling ---

(check-elaborates
  (labeled (variable 100) (type 0))
  (typed
    (type 1)
    (labeled (variable 100) (type 0))))

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

;; --- Neutral Variables ---

(check-elaborates
  (variable 100)
  (typed
    (typed (type 0) anything)
    (variable 100)))
