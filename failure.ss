(library (failure)
  (export
    failure failure? failure-value failure!
    failable-bind failable-let)
  (import
    (scheme)
    (data)
    (switch)
    (syntax))

  (data (failure value))

  (define-syntax-rule (failure! value)
    (failure (quote value)))

  (define (failable-bind $failable $fn)
    (switch $failable
      ((failure? $failure) $failure)
      ((else $success) ($fn $success))))

  (define-syntax-rule (failable-let ($success $failable) $body ...)
    (failable-bind $failable
      (lambda ($success)
        $body ...)))
)
