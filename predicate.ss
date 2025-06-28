(library (predicate)
  (export or? and?)
  (import (scheme) (syntax))

  (define-rule-syntax (or? pred ...)
    (lambda ($obj)
      (or (pred $obj) ...)))

  (define-rule-syntax (and? pred ...)
    (lambda ($obj)
      (and (pred $obj) ...)))
)
