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

; === evaluate native

(check-evaluates
  (native "foo")
  (evaluated (native "foo")))

; === evaluate typed native

(check-evaluates
  (typed nothing (native "foo"))
  (evaluated
    (typed nothing
      (evaluated (native "foo")))))

; === evaluate native-application

(check-evaluates
  (native-application string-append
    (list
      (typed nothing (native "foo"))
      (typed nothing (native "bar"))))
   (evaluated (native "foobar")))

; === evaluate typed native-application

(check-evaluates
  (typed nothing
    (native-application string-append
      (list
        (typed nothing (native "foo"))
        (typed nothing (native "bar")))))
  (evaluated
    (typed nothing
      (evaluated (native "foobar")))))

; === evaluate procedure?

(check-evaluates
  (lambda ($0) $0)
  (evaluated (lambda ($0) (evaluated $0))))

; === evaluate application

(check-evaluates
  (application
    (typed nothing (lambda ($0) $0))
    (typed nothing (native "foo")))
  (evaluated
    (typed nothing
      (evaluated (native "foo")))))

; === evaluate application

(check-evaluates
  (application
    (typed nothing (variable 0))
    (typed nothing (native "foo")))
  (evaluated
    (application
      (evaluated (typed nothing (evaluated (variable 0))))
      (evaluated (typed nothing (evaluated (native "foo")))))))

; === elab-task typed

(check-task=?
  (elab-task (typed (variable 10) (variable 20)))
  (task (typed (variable 10) (variable 20))))

; === elab-task native-type

(check-task=?
  (elab-task native-type)
  (task (typed (type 0) native-type)))

; === elab-task type

(check-task=?
  (elab-task (type 0))
  (task (type 0)))

; === elab-task native

(check-task=?
  (elab-task (native "foo"))
  (task (typed native-type (native "foo"))))

; === elab-task native-application

(check-task=?
  (elab-task
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
  (elab-task
    (native-application string-append
      (list
        (native "foo")
        (type 0))))
  (task ($solutions $errors)
    (values
      (list)
      (push $errors "not native")
      (typed native-type
        (native-application string-append
          (list
            (typed native-type (native "foo"))
            (type 0)))))))

; === typed

(check-elabs
  (typed (type 0) (native "foo"))
  (typed (type 0) (native "foo")))

; === ann

(check-elabs
  (ann (type 0) (typed (type 0) (native "foo")))
  (typed (type 0) (native "foo")))

(check-elabs
  (ann (type 1) (typed (type 0) (native "foo")))
  (typed
    (mismatch
      (expected (type 1))
      (actual (type 0)))
    (native "foo")))

; === native-type

(check-elabs
  native-type
  (typed (type 0) native-type))

; === native

(check-elabs
  (native "foo")
  (typed native-type (native "foo")))

; === native-application

(check-elabs
  (native-application string-append
    (list (native "foo") (native "bar")))
  (typed native-type
    (native-application string-append
    (list
      (typed native-type (native "foo"))
      (typed native-type (native "bar"))))))

; === type

(check-elabs
  (type 0)
  (typed (type 1) (type 0)))

(check-elabs
  (type 1)
  (typed (type 2) (type 1)))

; === signature

(check-elabs
  (signature
    (typed (type 0) (native "string"))
    (lambda (_)
      (typed (type 0) (native "number"))))
  (typed
    (type 0)
    (signature
      (typed (type 0) (native "string"))
      (lambda (_)
        (typed (type 0) (native "number"))))))

(check-elabs
  (signature
    (typed (type 0) (native "string"))
    (lambda ($arg) $arg))
  (typed
    (type 0)
    (signature
      (typed (type 0) (native "string"))
      (lambda (v0)
        (typed (type 0) v0)))))

; === procedure

(check-elabs
  (lambda ($arg) $arg)
  (typed
    (signature (hole 0) (lambda (v0) (hole 0)))
    (lambda (v0) (typed (hole 0) v0))))

(check-elabs
  (lambda ($arg) (typed (type 0) (native "string")))
  (typed
    (signature (hole 0) (lambda (v0) (type 0)))
    (lambda (v0) (typed (type 0) (native "string")))))

; === application

(check-elabs
  (application
    (lambda ($arg) $arg)
    (typed (type 0) (native "string")))
  (typed (type 0)
    (application
      (typed
        (signature (hole 0) (lambda (v0) (hole 0)))
        (lambda (v0) (typed (hole 0) v0)))
      (typed (type 0) (native "string")))))

(check-elabs
  (application
    (lambda (_) (typed (type 0) (native "number")))
    (typed (type 0) (native "string")))
  (typed
    (type 0)
    (application
      (typed
        (signature (hole 0) (lambda (_) (type 0)))
        (lambda (_) (typed (type 0) (native "number"))))
      (typed (type 0) (native "string")))))

(check-elabs
  (application
    (typed (type 0) (native "string"))
    (typed (type 0) (native "string")))
  (typed
    (typed (type 0) (application (type 0) (type 0)))
    (application
      (typed (type 0) (native "string"))
      (typed (type 0) (native "string")))))
