(library (leo syntax-case)
  (export syntax-case)
  (import
    (rename (scheme) (syntax-case %syntax-case))
    (syntax-keywords)
    (syntaxes))

  (define-rules-syntax
    (keywords keywords when)
    ((syntax-case expr (keywords k ...) (when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...)))
)
