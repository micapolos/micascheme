(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate)
  (leo2 stdlib))

; TODO: Rewrite `elaborate to use `deduce

; ; --- typed

; (check-elaborates
;   (typed (type 2) (native "foo"))
;   (typed (type 2) (native "foo")))

; ; --- type

; (check-elaborates
;   (type 12)
;   (type 12))

; ; --- native

; (check-elaborate-throws
;   (native "foo"))

; (check-elaborate-throws
;   (application
;     (lambda-type (type 1) (lambda (x) x))
;     (native "not-a-type-0")))

; ; --- evaluated

; (check-elaborates
;   (evaluated (type 0))
;   (type 0))

; ; --- native-application

; (check-elaborates
;   (typed
;     (native boolean-type)
;     (native-application zero? (list (variable 100))))
;   (typed
;     (native boolean-type)
;     (native-application zero? (list (variable 100)))))

; (check-elaborates
;   (native-application zero? (list (variable 100)))
;   (typed
;     (typed (type 0) (hole 0))
;     (native-application zero?
;       (list
;         (typed
;           (typed (type 0) (hole 0))
;           (variable 100))))))

; ; --- variable

; (check-elaborates
;   (variable 100)
;   (typed
;     (typed (type 0) (hole 0))
;     (variable 100)))

; ; --- lambda

; (check-elaborate-throws
;   (lambda (x) x))

; ; --- lambda-type

; (check-elaborates
;   (lambda-type (type 0)
;     (lambda (x) x))
;   (typed
;     (typed
;       (type 0)
;       (lambda-type
;         (type 0)
;         (lambda (type 0))))
;     (lambda-type (type 0)
;       (lambda
;         (typed
;           (type 0)
;           (variable 0))))))

; ; --- application

; (check-elaborates
;   (application
;     (lambda (x) x)
;     (type 50))
;   (typed
;     (type 51)
;     (application
;       (typed
;         (typed
;           (type 0)
;           (lambda-type
;             (type 51)
;             (lambda (type 51))))
;         (lambda-type (type 51)
;           (lambda
;             (typed
;               (type 51)
;               (variable 0)))))
;       (type 50))))

; ; --- branch

; ; Case 1: Matching types
; (check-elaborates
;   (branch
;     (typed (type 0) (native boolean-type))
;     (type 0)
;     (type 0))
;   (typed
;     (branch
;       (native boolean-type)
;       (type 1)
;       (type 1))
;     (branch
;       (typed (type 0) (native boolean-type))
;       (type 0)
;       (type 0))))

; ; Case 2: Mismatching types (Dependent Type Selection)
; (check-elaborates
;   (branch
;     (typed (type 0) (native boolean-type))
;     (type 0)
;     (type 1))
;   (typed
;     (branch
;       (native boolean-type)
;       (type 1)
;       (type 2))
;     (branch
;       (typed (type 0) (native boolean-type))
;       (type 0)
;       (type 1))))

; ; Case 3: Neutral condition
; (check-elaborates
;   (branch
;     (variable 100)
;     (type 0)
;     (type 0))
;   (typed
;     (branch
;       (variable 100)
;       (type 1)
;       (type 1))
;     (branch
;       (typed (typed (type 0) (hole 0)) (variable 100))
;       (type 0)
;       (type 0))))

; ; --- recursion

; (check-elaborates
;   (recursion
;     (lambda-type (type 0)
;       (lambda (x) x)))
;   (typed
;     (typed
;       (type 0)
;       (lambda-type (type 0) (lambda (type 0))))
;     (recursion
;       (lambda-type (type 0)
;         (lambda
;           (typed
;             (type 0)
;             (variable 0)))))))

; ; --- labeled

; (check-elaborates
;   (labeled (variable 100) (type 0))
;   (typed
;     (type 1)
;     (labeled (variable 100) (type 0))))
