(library (failure)
  (export
    failure failure? failure-value failure!
    failable failable-bind)
  (import
    (scheme)
    (data)
    (switch)
    (monad)
    (syntax))

  (data (failure value))

  (define-syntax-rule (failure! value)
    (failure (quote value)))

  (define-monad failable
    ((pure $value) $value)
    ((bind $failable $fn)
      (switch $failable
        ((failure? $failure) $failure)
        ((else $success) ($fn $success)))))
)
