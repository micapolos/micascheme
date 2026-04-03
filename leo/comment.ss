(library (leo comment)
  (export comment)
  (import
    (scheme)
    (syntax))

  (define-rule-syntax (comment x ...) (begin))
)
