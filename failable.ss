(library (failable)
  (export
    failable
    failable-failure

    failable-bind
    failable-bind-failure

    failable-recover)
  (import
    (scheme)
    (binder)
    (failure)
    (monad)
    (switch)
    (lets)
    (syntax))

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

  (define-bind failable-failure
    (syntax-rules ()
      ((_ ($failable $value) $body)
        (switch $failable
          ((failure? $failure)
            (lets
              ($value (failure-value $failure))
              $body))
          ((else $success) $success)))))

  (define (failable-recover $failable $fn)
    (switch $failable
      ((failure? $failure) ($fn $failure))
      ((else $success) $success)))
)
