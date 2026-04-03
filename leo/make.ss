(library (leo make)
  (export make)
  (import
    (scheme)
    (leo maker)
    (syntax))

  (define-rule-syntax (make (id expr ...))
    ((maker id) expr ...))
)
