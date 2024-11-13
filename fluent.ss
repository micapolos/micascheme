(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntaxes))

  (define-rules-syntax
    ((fluent expr) expr)
    ((fluent expr (fn arg ...) rest ...)
      (let ((var expr))
        (fluent (fn var arg ...) rest ...)))))
