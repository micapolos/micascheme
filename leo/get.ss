(library (leo get)
  (export get)
  (import
    (scheme)
    (leo getter-leo)
    (syntax))

  (define-rule-syntax (get (target (id expr ...)))
    ((getter (target id)) expr ...))
)
