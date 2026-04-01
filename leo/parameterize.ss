(library (leo parameterize)
  (export parameterize)
  (import
    (rename (scheme)
      (parameterize %parameterize))
    (leo in)
    (syntaxes))

  (define-rules-syntax
    (keywords in)

    ((parameterize binding ... (in x xs ...))
      (%parameterize (binding ...) x xs ...)))
)
