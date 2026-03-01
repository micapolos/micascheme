(import
  (leo2 base)
  (leo2 term)
  (leo2 elab))

; === task

(check-task=?
  (task "foo")
  (task "foo"))

; === task-lets

(check-task=?
  (task-lets (task "foo"))
  (task "foo"))

(check-task=?
  (task-lets
    ($foo (task "foo"))
    ($bar (task "bar"))
    (task (string-append $foo $bar)))
  (task "foobar"))

; === list->task

(check-task=?
  (list->task (list (task (native "foo")) (task (native "bar"))))
  (task (list (native "foo") (native "bar"))))

; ==================== solve-task =======================

; native-type
(check-task=?
  (solve-task empty-env native-type native-type)
  (task native-type))

(check-task=?
  (solve-task empty-env native-type (variable 0))
  (push-error-task
    (mismatch
      (expected native-type)
      (actual (variable 0)))
    nothing))

; type
(check-task=?
  (solve-task empty-env (type 0) (type 0))
  (task (type 0)))

(check-task=?
  (solve-task empty-env (type 0) (type 1))
  (push-error-task
    (mismatch
      (expected (type 0))
      (actual (type 1)))
    nothing))

(check-task=?
  (solve-task empty-env (type 0) (variable 0))
  (push-error-task
    (mismatch
      (expected (type 0))
      (actual (variable 0)))
    nothing))

; ==================== eval-task =======================

; === eval-task native

(check-task=?
  (eval-task empty-env (evaluated (native "foo")))
  (task (evaluated (native "foo"))))

; === eval-task type

(check-task=?
  (eval-task empty-env (type 10))
  (task (evaluated (type 10))))

; === eval-task native-type

(check-task=?
  (eval-task empty-env native-type)
  (task (evaluated native-type)))

; === eval-task native

(check-task=?
  (eval-task empty-env (native "foo"))
  (task (evaluated (native "foo"))))

; === eval-task typed

(check-task=?
  (eval-task empty-env
    (typed native-type (native "foo")))
  (task
    (evaluated
      (typed
        (evaluated native-type)
        (evaluated (native "foo"))))))

; === eval-task native-application

(check-task=?
  (eval-task empty-env
    (native-application string-append
      (list
        (typed native-type (native "foo"))
        (typed native-type (native "bar")))))
  (task (evaluated (native "foobar"))))

; === eval-task variable

(check-task=?
  (eval-task (stack (native "foo")) (variable 0))
  (task (evaluated (variable 0))))

; === eval-task lambda

(check-task=?
  (eval-task empty-env
    (lambda ($0) $0))
  (task
    (evaluated
      (lambda ($0)
        (evaluated $0)))))

; === eval-task lambda-type

(check-task=?
  (eval-task empty-env
    (lambda-type (type 0)
      (lambda ($0) $0)))
  (task
     (evaluated
      (lambda-type
        (evaluated (type 0))
        (evaluated (lambda ($0) (evaluated $0)))))))

; === eval-task application

(check-task=?
  (eval-task empty-env
    (application
      (typed native-type (lambda ($0) $0))
      (typed native-type (native "foo"))))
  (task
    (evaluated
      (typed
        (evaluated native-type)
        (evaluated (native "foo"))))))

; ==================== eval-task =======================

; === elab-task typed

(check-task=?
  (elab-task empty-env (typed (variable 10) (variable 20)))
  (task (typed (variable 10) (variable 20))))

; === elab-task ann

(check-task=?
  (elab-task empty-env (ann (type 1) (type 0)))
  (task (typed (type 1) (type 0))))

(check-task=?
  (elab-task empty-env (ann (type 2) (type 0)))
  (push-error-task
    (mismatch
      (expected (type 2))
      (actual (type 1)))
    (typed nothing (type 0))))

; === elab-task native-type

(check-task=?
  (elab-task empty-env native-type)
  (task (typed (type 0) native-type)))

; === elab-task type

(check-task=?
  (elab-task empty-env (type 0))
  (task (type 0)))

; === elab-task native

(check-task=?
  (elab-task empty-env (native "foo"))
  (task (typed native-type (native "foo"))))

; === elab-task native-application

(check-task=?
  (elab-task empty-env
    (native-application string-append
      (list
        (native "foo")
        (native "bar"))))
  (task
    (typed native-type
      (native-application string-append
      (list
        (typed native-type (native "foo"))
        (typed native-type (native "bar")))))))

(check-task=?
  (elab-task empty-env
    (native-application string-append
      (list
        (native "foo")
        (type 0))))
  (push-error-task
    (mismatch
      (expected native-type)
      (actual (type 1)))
    (typed nothing
      (native-application string-append
        (list
          (typed native-type (native "foo"))
          (typed nothing (type 0)))))))

; === elab-task variable

(check-task=?
  (elab-task (stack (type 10) (type 20)) (variable 0))
  (task (typed (type 20) (variable 0))))

(check-task=?
  (elab-task (stack (type 10) (type 20)) (variable 1))
  (task (typed (type 10) (variable 1))))

(check-task=?
  (elab-task (stack (type 10) (type 20)) (variable 2))
  (task
    (solutions)
    (errors "unbound variable")
    (typed nothing (variable 2))))

; === elab-task lambda-type

(check-task=?
  (elab-task empty-env
    (lambda-type (type 1)
      (lambda ($0) (type 2))))
  (task
    (typed (type 3)
      (lambda-type (type 1)
        (lambda ($0) (type 2))))))

(check-task=?
  (elab-task empty-env
    (lambda-type (type 1)
      (lambda ($0) $0)))
  (task
    (typed (type 2)
      (lambda-type (type 1)
        (lambda ($0)
          (typed (type 1) $0))))))

; === elab-task lambda

(check-task=?
  (elab-task empty-env (lambda ($0) $0))
  (task
    (solutions unknown)
    (errors)
    (typed
      (lambda-type (hole 0) (lambda ($0) (hole 0)))
      (lambda ($0) (typed (hole 0) $0)))))

(check-task=?
  (elab-task empty-env
    (lambda ($arg) (native "string")))
  (task
    (solutions unknown)
    (errors)
    (typed
      (lambda-type (hole 0)
        (lambda ($0) native-type))
      (lambda ($0)
        (typed native-type (native "string"))))))

; === elab-task application

(check-task=?
  (elab-task empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda (_) (type 2)))
        (variable 0))
      (type 0)))
  (task
    (typed (type 2)
      (application
        (typed
          (lambda-type (type 1)
            (lambda (_) (type 2)))
          (variable 0))
        (type 0)))))

(check-task=?
  (elab-task empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda ($0) $0))
        (variable 0))
      (type 0)))
  (task
    (typed (type 0)
      (application
        (typed
          (lambda-type (type 1)
            (lambda ($0) $0))
          (variable 0))
        (type 0)))))

(check-task=?
  (elab-task empty-env
    (application
      (typed
        (lambda-type (type 1)
          (lambda ($0) $0))
        (variable 0))
      (type 2)))
  (push-error-task
    (mismatch
      (expected (type 1))
      (actual (type 3)))
    (typed nothing
      (application
        (typed
          (lambda-type (type 1)
            (lambda ($0) $0))
          (variable 0))
        (type 2)))))

; === eval-task ann lambda

; (check-task=?
;   (elab-task empty-env
;     (ann
;       (lambda-type native-type
;         (lambda ($0) native-type))
;       (lambda ($0) $0)))
;   (task 123))
