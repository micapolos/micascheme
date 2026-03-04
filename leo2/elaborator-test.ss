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

; ==================== term-elaborator =======================

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
  (elaborator
    (typed
      (evaluated (mismatch (type 2) (type 1)))
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
  (elaborator
    (typed nothing
      (native-application string-append
        (list
          (typed
            (evaluated native-type)
            (native "foo"))
          (typed
            (evaluated (mismatch native-type (type 1)))
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
  (elaborator
    (typed (type 2)
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
