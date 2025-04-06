(library (list-syntax)
  (export map-with for for-all-with exists-with)
  (import (scheme) (syntax))

  (define-rule-syntax (map-with (id list) ... body)
    (map (lambda (id ...) body) list ...))

  (define-rule-syntax (for (id list) body ...)
    (for-each (lambda (id) body ...) list))

  (define-rule-syntax (for-all-with (id list) ... body)
    (for-all (lambda (id ...) body) list ...))

  (define-rule-syntax (exists-with (id list) ... body)
    (exists (lambda (id ...) body) list ...))
)
