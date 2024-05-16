(library (let)
  (export let-values/optimize)
  (import (scheme) (syntaxes))

  (define-rules-syntax
    ((let-values/optimize ((($var) $expr) ...) $body ...)
      (let (($var $expr) ...) $body ...))
    ((let-values/optimize ((($var ...) $expr) ...) $body ...)
      (let-values ((($var ...) $expr) ...) $body ...)))
)
