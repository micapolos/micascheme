(import
  (leo2 base)
  (leo2 term)
  (leo2 elab))

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
  (typed nothing (native "foo")))

(check-elabs
  (ann (type 1) (typed (type 0) (native "foo")))
  (typed nothing (native "foo")))

; === native

(check-elabs
  (native "foo")
  (typed nothing (native "foo")))

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
      (lambda
        (typed (type 0) (native "number"))))))

(check-elabs
  (signature
    (typed (type 0) (native "string"))
    (lambda ($arg) $arg))
  (typed
    (type 0)
    (signature
      (typed (type 0) (native "string"))
      (lambda
        (typed (type 0) (variable 0))))))

; === procedure

(check-elabs
  (lambda ($arg) $arg)
  (typed
    (signature (hole 0) (lambda (hole 0)))
    (lambda (typed (hole 0) (variable 0)))))

(check-elabs
  (lambda ($arg) (typed (type 0) (native "string")))
  (typed
    (signature (hole 0) (lambda (type 0)))
    (lambda (typed (type 0) (native "string")))))

; === application

(check-elabs
  (application
    (lambda ($arg) $arg)
    (typed (type 0) (native "string")))
  (typed (type 0)
    (application
      (typed
        (signature (hole 0) (lambda (hole 0)))
        (lambda (typed (hole 0) (variable 0))))
      (typed (type 0) (native "string")))))

(check-elabs
  (application
    (lambda (_) (typed (type 0) (native "number")))
    (typed (type 0) (native "string")))
  (typed
    (type 0)
    (application
      (typed
        (signature (hole 0) (lambda (type 0)))
        (lambda (typed (type 0) (native "number"))))
      (typed (type 0) (native "string")))))

(check-elabs
  (application
    (typed (type 0) (native "string"))
    (typed (type 0) (native "string")))
  (typed nothing
    (application
      (typed (type 0) (native "string"))
      (typed (type 0) (native "string")))))
