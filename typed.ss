(library (typed)
  (export
    typed typed? typed-value typed-type
    native native? native-value native-type
    variable variable? variable-index v0 v1 v2
    function function? function-params function-body function!
    application application? application-fn application-args application!)

  (import (micascheme) (type))

  (data (typed value type))
  (data (native value type))
  (data (variable index))
  (data (function params body))
  (data (application fn args))

  (define-syntax v0
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 0)))))

  (define-syntax v1
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 1)))))

  (define-syntax v2
    (lambda (stx)
      (syntax-case stx ()
        (_ #`(variable 2)))))

  (define-syntax function!
    (syntax-rules ()
      ((_ param ... body)
        (function (list param ...) body))))

  (define-syntax application!
    (syntax-rules ()
      ((_ fn arg ...)
        (application fn (list arg ...)))))
)