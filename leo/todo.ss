(library (leo todo)
  (export todo)
  (import
    (scheme)
    (syntax))

  (define-rule-syntax (todo x ...) (void))
)
