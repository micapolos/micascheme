(library (leo parameterize)
  (export parameterize)
  (import
    (rename (scheme)
      (parameterize %parameterize))
    (leo in)
    (syntaxes))

  (define-rules-syntax
    (keywords in)

    ((parameterize (id expr) ... (in x xs ...))
      (%parameterize ((id expr) ...) x xs ...)))
)
