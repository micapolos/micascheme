(library (failure)
  (export
    failure failure? failure-value failure!
    fallible-bind fallible-let)
  (import
    (scheme)
    (data)
    (switch)
    (syntax))

  (data (failure value))

  (define-syntax-rule (failure! value)
    (failure (quote value)))

  (define (fallible-bind $fallible $fn)
    (switch $fallible
      ((failure? $failure) $failure)
      ((else $success) ($fn $success))))

  (define-syntax-rule (fallible-let ($success $fallible) $body ...)
    (fallible-bind $fallible
      (lambda ($success)
        $body ...)))
)
