(library (list-syntax)
  (export map-with)
  (import (scheme) (syntax))

  (define-rule-syntax (map-with (id list) ... body)
    (map (lambda (id ...) body) list ...))
)
