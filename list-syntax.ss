(library (list-syntax)
  (export map-with for)
  (import (scheme) (syntax))

  (define-rule-syntax (map-with (id list) ... body)
    (map (lambda (id ...) body) list ...))

  (define-rule-syntax (for (id list) body ...)
    (for-each (lambda (id) body ...) list))
)
