(library (define)
  (export define-recursive)
  (import (scheme) (syntax) (procedure))

  (define-rule-syntax (define-recursive (id . params) body ...)
    (define id (recursive-lambda (id . params) body ...)))
)
