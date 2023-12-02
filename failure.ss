(library (failure)
  (export
    failure failure? failure-value
    failable
    failable-bind
    failable-bind-failure
    failable-failure
    failable-recover)
  (import
    (scheme)
    (data)
    (switch)
    (monad)
    (function)
    (stack)
    (binder)
    (syntax))

  (data (failure value))

  (define-monad failable
    ((pure $value) $value)
    ((bind $failable $fn)
      (switch $failable
        ((failure? $failure) $failure)
        ((else $success) ($fn $success)))))

  (define (failable-bind-failure $failable $fn)
    (switch $failable
      ((failure? $failure) ($fn $failure))
      ((else $success) $success)))

  (define-aux-keyword failable-failure)

  (define-binder failable-failure
    (lambda ($failable $fn)
      (switch $failable
        ((failure? $failure) ($fn (failure-value $failure)))
        ((else $success) $success))))

  (define (failable-recover $failable $fn)
    (switch $failable
      ((failure? $failure) ($fn $failure))
      ((else $success) $success)))
)
