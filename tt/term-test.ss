(import
  (scheme)
  (check)
  (tt term))

;; Variable index strictly greater than or equal to cutoff: SHIFT
(check
  (equal?
    (term-shift 2 0 (variable 10))
    (variable 12)))

(check
  (equal?
    (term-shift 3 2 (variable 2))
    (variable 5)))

(check
  (equal?
    (term-shift 3 5 (variable 2))
    (variable 2)))

(check
  (equal?
    (term-shift -2 0 (variable 12))
    (variable 10)))

(check
  (equal?
    (term-shift 2 0 "foo")
    "foo"))

(check
  (equal?
    (term-shift 2 0 (kind 'Type))
    (kind 'Type)))

(check
  (equal?
    (term-shift 2 0 (hole 0 1 10))
    (hole 0 1 10)))

(check
  (equal?
    (term-shift 2 0
      (abstraction
        (tuple-constructor
          (list (variable 0) (variable 1)))))
    (abstraction
      (tuple-constructor
        (list (variable 0) (variable 3))))))

(check
  (equal?
    (term-shift 2 0
      (pi
        (variable 0)   ; Free at cutoff 0 -> shifted to 2
        (variable 0))) ; Bound at cutoff 1 -> unshifted
    (pi
      (variable 2)
      (variable 0))))

(check
  (equal?
    (term-shift 2 0
      (application (variable 0) (variable 1)))
    (application (variable 2) (variable 3))))

(check
  (equal?
    (term-shift 2 0
      (type-constructor 'List (list (variable 0))))
    (type-constructor 'List (list (variable 2)))))

(check
  (equal?
    (term-shift 2 0
      (tuple-constructor (list (variable 0) "foo")))
    (tuple-constructor (list (variable 2) "foo"))))

(check
  (equal?
    (term-shift 2 0
      (tuple-projection (variable 1) 0))
    (tuple-projection (variable 3) 0)))

(check
  (equal?
    (term-shift 2 0
      (union-constructor 0 (variable 1)))
    (union-constructor 0 (variable 3))))

(check
  (equal?
    (term-shift 2 0
      (union-eliminator
        (variable 0)
        (list (variable 1) (variable 2))))
    (union-eliminator
      (variable 2)
      (list (variable 3) (variable 4)))))

(check
  (equal?
    (term-shift 2 0
      (primitive-application '+ (list (variable 0) (variable 1))))
    (primitive-application '+ (list (variable 2) (variable 3)))))
