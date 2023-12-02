(library (failure)
  (export
    failure failure? failure-items
    make-failure
    failure-push
    failable failable-bind failable-with)
  (import
    (scheme)
    (data)
    (switch)
    (monad)
    (function)
    (stack)
    (syntax))

  (data (failure . items))

  (define-monad failable
    ((pure $value) $value)
    ((bind $failable $fn)
      (switch $failable
        ((failure? $failure) $failure)
        ((else $success) ($fn $success)))))

  (function (failure-push (failure $items) $entry)
    (make-failure (push $items $entry)))

  (define-syntax-rule (failable-with $item $failable)
    (switch $failable
      ((failure? $failure) (failure-push $failure ((lambda () $item))))
      ((else $success) $success)))
)
