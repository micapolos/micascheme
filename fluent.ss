(library (fluent)
  (export fluent with)
  (import
    (scheme) (syntax))

  (define-aux-keyword with)

  (define-syntax fluent
    (syntax-rules (with values)
      ((fluent expr)
        expr)
      ((fluent (values value ...) (fn arg ...) rest ...)
        (fluent (fn value ... arg ...) rest ...))
      ((fluent expr (with with-var with-expr) rest ...)
        (let ((with-var expr))
          (fluent with-expr rest ...)))
      ((fluent expr (fn arg ...) rest ...)
        (fluent (fn expr arg ...) rest ...)))))
