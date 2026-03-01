(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborator))

; === elaborator

(check-elaborator=?
  (elaborator "foo")
  (elaborator "foo"))

; === elaborator-lets

(check-elaborator=?
  (elaborator-lets (elaborator "foo"))
  (elaborator "foo"))

(check-elaborator=?
  (elaborator-lets
    ($foo (elaborator "foo"))
    ($bar (elaborator "bar"))
    (elaborator (string-append $foo $bar)))
  (elaborator "foobar"))

; === list->elaborator

(check-elaborator=?
  (list->elaborator (list (elaborator (native "foo")) (elaborator (native "bar"))))
  (elaborator (list (native "foo") (native "bar"))))

; ==================== solve-elaborator =======================

; rhs hole
(check-solutions-elaborator=?
  (stack native-type)
  (solve-elaborator empty-env native-type (hole 0))
  (elaborator
    (solutions native-type)
    (errors)
    native-type))

; hole
(check-elaborator=?
  (solve-elaborator empty-env (hole 0) native-type)
  (elaborator
    (solutions)
    (errors (unbound (hole 0)))
    nothing))

(check-solutions-elaborator=?
  (stack native-type)
  (solve-elaborator empty-env (hole 0) native-type)
  (elaborator
    (solutions native-type)
    (errors)
    native-type))

(check-solutions-elaborator=?
  (stack (variable 0))
  (solve-elaborator empty-env (hole 0) native-type)
  (elaborator
    (solutions (variable 0))
    (errors (mismatch (expected (variable 0)) (actual native-type)))
    nothing))

; unknown
(check-elaborator=?
  (solve-elaborator empty-env unknown unknown)
  (elaborator unknown))

(check-elaborator=?
  (solve-elaborator empty-env unknown native-type)
  (elaborator native-type))

; native-type
(check-elaborator=?
  (solve-elaborator empty-env native-type native-type)
  (elaborator native-type))

(check-elaborator=?
  (solve-elaborator empty-env native-type (variable 0))
  (push-error-elaborator
    (mismatch
      (expected native-type)
      (actual (variable 0)))
    nothing))

; type
(check-elaborator=?
  (solve-elaborator empty-env (type 0) (type 0))
  (elaborator (type 0)))

(check-elaborator=?
  (solve-elaborator empty-env (type 0) (type 1))
  (push-error-elaborator
    (mismatch
      (expected (type 0))
      (actual (type 1)))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env (type 0) (variable 0))
  (push-error-elaborator
    (mismatch
      (expected (type 0))
      (actual (variable 0)))
    nothing))

; native
(check-elaborator=?
  (solve-elaborator empty-env (native "foo") (native "foo"))
  (elaborator (native "foo")))

(check-elaborator=?
  (solve-elaborator empty-env (native "foo") (native "bar"))
  (push-error-elaborator
    (mismatch
      (expected (native "foo"))
      (actual (native "bar")))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env (variable 0) (native "bar"))
  (push-error-elaborator
    (mismatch
      (expected (variable 0))
      (actual (native "bar")))
    nothing))

; evaluated
(check-elaborator=?
  (solve-elaborator empty-env
    (evaluated (native "foo"))
    (evaluated (native "foo")))
  (elaborator (evaluated (native "foo"))))

(check-elaborator=?
  (solve-elaborator empty-env
    (evaluated (native "foo"))
    (evaluated (native "bar")))
  (push-error-elaborator
    (mismatch
      (expected (native "foo"))
      (actual (native "bar")))
    nothing))

; typed
(check-elaborator=?
  (solve-elaborator empty-env
    (typed (native "t1") (native "v1"))
    (typed (native "t1") (native "v1")))
  (elaborator
    (typed (native "t1") (native "v1"))))

(check-elaborator=?
  (solve-elaborator empty-env
    (typed (native "t1") (native "v1"))
    (typed (native "t1") (native "v2")))
  (push-error-elaborator
    (mismatch (expected (native "v1")) (actual (native "v2")))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env
    (typed (native "t1") (native "v1"))
    (typed (native "t2") (native "v1")))
  (push-error-elaborator
    (mismatch (expected (native "t1")) (actual (native "t2")))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env
    (typed (native "t1") (native "v1"))
    (typed (native "t2") (native "v2")))
  (elaborator
    (solutions)
    (errors
      (mismatch (expected (native "t1")) (actual (native "t2")))
      (mismatch (expected (native "v1")) (actual (native "v2"))))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env
    (typed (native "t1") (native "v1"))
    (native "v1"))
  (elaborator
    (solutions)
    (errors
      (mismatch
        (expected (typed (native "t1") (native "v1")))
        (actual (native "v1"))))
    nothing))

; lambda
(check-elaborator=?
  (solve-elaborator empty-env
    (lambda ($0) $0)
    (lambda ($0) $0))
  (elaborator (lambda ($0) $0)))

(check-elaborator=?
  (solve-elaborator empty-env
    (lambda ($0) (lambda ($1) $0))
    (lambda ($0) (lambda ($1) $0)))
  (elaborator (lambda ($0) (lambda ($1) $0))))

(check-elaborator=?
  (solve-elaborator empty-env
    (lambda ($0) (lambda ($1) $1))
    (lambda ($0) (lambda ($1) $1)))
  (elaborator (lambda ($0) (lambda ($1) $1))))

(check-elaborator=?
  (solve-elaborator empty-env
    (lambda ($0) (lambda ($1) $1))
    (lambda ($0) (lambda ($1) $0)))
  (push-error-elaborator
    (mismatch
      (expected (variable 1))
      (actual (variable 0)))
    nothing))

(check-elaborator=?
  (solve-elaborator empty-env
    (lambda ($0) $0)
    (variable 0))
  (push-error-elaborator
    (mismatch
      (expected (lambda ($0) $0))
      (actual (variable 0)))
    nothing))

; lambda-type
(check-elaborator=?
  (solve-elaborator empty-env
    (lambda-type native-type (lambda ($0) $0))
    (lambda-type native-type (lambda ($0) $0)))
  (elaborator
    (lambda-type native-type
      (lambda ($0) $0))))

(check-elaborator=?
  (solve-elaborator empty-env
    (lambda-type native-type (lambda ($0) $0))
    (lambda-type native-type (lambda ($0) native-type)))
  (push-error-elaborator
    (mismatch
      (expected (variable 0))
      (actual native-type))
    nothing))

; ==================== eval-elaborator =======================

; === eval-elaborator native

(check-elaborator=?
  (eval-elaborator empty-env (evaluated (native "foo")))
  (elaborator (evaluated (native "foo"))))

; === eval-elaborator type

(check-elaborator=?
  (eval-elaborator empty-env (type 10))
  (elaborator (evaluated (type 10))))

; === eval-elaborator native-type

(check-elaborator=?
  (eval-elaborator empty-env native-type)
  (elaborator (evaluated native-type)))

; === eval-elaborator native

(check-elaborator=?
  (eval-elaborator empty-env (native "foo"))
  (elaborator (evaluated (native "foo"))))

; === eval-elaborator typed

(check-elaborator=?
  (eval-elaborator empty-env
    (typed native-type (native "foo")))
  (elaborator
    (evaluated
      (typed
        (evaluated native-type)
        (evaluated (native "foo"))))))

; === eval-elaborator native-application

(check-elaborator=?
  (eval-elaborator empty-env
    (native-application string-append
      (list
        (typed native-type (native "foo"))
        (typed native-type (native "bar")))))
  (elaborator (evaluated (native "foobar"))))

; === eval-elaborator variable

(check-elaborator=?
  (eval-elaborator (stack (native "foo")) (variable 0))
  (elaborator (evaluated (variable 0))))

; === eval-elaborator lambda

(check-elaborator=?
  (eval-elaborator empty-env
    (lambda ($0) $0))
  (elaborator
    (evaluated
      (lambda ($0)
        (evaluated $0)))))

; === eval-elaborator lambda-type

(check-elaborator=?
  (eval-elaborator empty-env
    (lambda-type (type 0)
      (lambda ($0) $0)))
  (elaborator
     (evaluated
      (lambda-type
        (evaluated (type 0))
        (evaluated (lambda ($0) (evaluated $0)))))))

; === eval-elaborator application

(check-elaborator=?
  (eval-elaborator empty-env
    (application
      (typed native-type (lambda ($0) $0))
      (typed native-type (native "foo"))))
  (elaborator
    (evaluated
      (typed
        (evaluated native-type)
        (evaluated (native "foo"))))))

; ==================== eval-elaborator =======================

; === term-elaborator typed

(check-elaborator=?
  (term-elaborator empty-env (typed (variable 10) (variable 20)))
  (elaborator (typed (variable 10) (variable 20))))

; === term-elaborator ann

(check-elaborator=?
  (term-elaborator empty-env (ann (type 1) (type 0)))
  (elaborator
    (typed
      (evaluated (type 1))
      (type 0))))

(check-elaborator=?
  (term-elaborator empty-env (ann (type 2) (type 0)))
  (push-error-elaborator
    (mismatch
      (expected (type 2))
      (actual (type 1)))
    (typed
      (evaluated nothing)
      (type 0))))

; === term-elaborator native-type

(check-elaborator=?
  (term-elaborator empty-env native-type)
  (elaborator (typed (type 0) native-type)))

; === term-elaborator type

(check-elaborator=?
  (term-elaborator empty-env (type 0))
  (elaborator (type 0)))

; === term-elaborator native

(check-elaborator=?
  (term-elaborator empty-env (native "foo"))
  (elaborator (typed native-type (native "foo"))))

; === term-elaborator native-application

(check-elaborator=?
  (term-elaborator empty-env
    (native-application string-append
      (list
        (native "foo")
        (native "bar"))))
  (elaborator
    (typed (evaluated native-type)
      (native-application string-append
      (list
        (typed
          (evaluated native-type)
          (native "foo"))
        (typed
          (evaluated native-type)
          (native "bar")))))))

(check-elaborator=?
  (term-elaborator empty-env
    (native-application string-append
      (list
        (native "foo")
        (type 0))))
  (push-error-elaborator
    (mismatch
      (expected native-type)
      (actual (type 1)))
    (typed nothing
      (native-application string-append
        (list
          (typed
            (evaluated native-type)
            (native "foo"))
          (typed
            (evaluated nothing)
            (type 0)))))))

; === term-elaborator variable

(check-elaborator=?
  (term-elaborator (stack (type 10) (type 20)) (variable 0))
  (elaborator (typed (type 20) (variable 0))))

(check-elaborator=?
  (term-elaborator (stack (type 10) (type 20)) (variable 1))
  (elaborator (typed (type 10) (variable 1))))

(check-elaborator=?
  (term-elaborator (stack (type 10) (type 20)) (variable 2))
  (push-error-elaborator
    (unbound (variable 2))
    (typed nothing (variable 2))))

; === term-elaborator lambda-type

(check-elaborator=?
  (term-elaborator empty-env
    (lambda-type (type 1)
      (lambda ($0) (type 2))))
  (elaborator
    (typed (type 3)
      (lambda-type (type 1)
        (lambda ($0) (type 2))))))

(check-elaborator=?
  (term-elaborator empty-env
    (lambda-type (type 1)
      (lambda ($0) $0)))
  (elaborator
    (typed (type 2)
      (lambda-type (type 1)
        (lambda ($0)
          (typed (type 1) $0))))))

; === term-elaborator lambda

(check-elaborator=?
  (term-elaborator empty-env (lambda ($0) $0))
  (elaborator
    (solutions unknown)
    (errors)
    (typed
      (lambda-type (hole 0) (lambda ($0) (hole 0)))
      (lambda ($0) (typed (hole 0) $0)))))

(check-elaborator=?
  (term-elaborator empty-env
    (lambda ($arg) (native "string")))
  (elaborator
    (solutions unknown)
    (errors)
    (typed
      (lambda-type (hole 0)
        (lambda ($0) native-type))
      (lambda ($0)
        (typed native-type (native "string"))))))

; === term-elaborator application

(check-elaborator=?
  (term-elaborator empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda (_) (type 2)))
        (variable 0))
      (type 0)))
  (elaborator
    (typed (type 2)
      (application
        (typed
          (lambda-type (type 1)
            (lambda (_) (type 2)))
          (variable 0))
        (type 0)))))

(check-elaborator=?
  (term-elaborator empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda ($0) $0))
        (variable 0))
      (type 0)))
  (elaborator
    (typed (type 0)
      (application
        (typed
          (lambda-type (type 1)
            (lambda ($0) $0))
          (variable 0))
        (type 0)))))

(check-elaborator=?
  (term-elaborator empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda ($0) $0))
        (variable 0))
      (type 2)))
  (push-error-elaborator
    (mismatch
      (expected (type 1))
      (actual (type 3)))
    (typed
      (evaluated nothing)
      (application
        (typed
          (lambda-type (type 1)
            (lambda ($0) $0))
          (variable 0))
        (type 2)))))

; === eval-elaborator ann lambda

; (check-elaborator=?
;   (term-elaborator empty-env
;     (ann
;       (lambda-type native-type
;         (lambda ($0) native-type))
;       (lambda ($0) $0)))
;   (elaborator
;     (typed
;       (lambda-type native-type
;         (lambda ($0) native-type))
;       (lambda ($0) $0))))
