(library (leo with)
  (export with)
  (import (scheme) (syntax))

  (define-rule-syntax (with x ...) (x ...))
)
