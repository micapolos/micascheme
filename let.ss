(library (let)
  (export let-values/optimize)
  (import (scheme))

  (define-syntax let-values/optimize
    (syntax-rules ()
      ((_ ((() $expr) ...) $body ...)
        (let () $body ...))
      ((_ ((($var) $expr) ...) $body ...)
        (let (($var $expr) ...) $body ...))
      ((_ ((($var ...) $expr) ...) $body ...)
        (let-values ((($var ...) $expr) ...) $body ...))))
)
