(library (predicate)
  (export or-predicate)
  (import (scheme) (syntax))

  (define-rule-syntax (or-predicate pred ...)
    (lambda ($obj)
      (or (pred $obj) ...)))
)
