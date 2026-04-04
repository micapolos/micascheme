(library (leo syntax-case)
  (export syntax-case)
  (import
    (rename (scheme) (syntax-case %syntax-case))
    (keyword)
    (syntax-keywords)
    (syntaxes))

  (define-rules-syntax
    (keywords keywords when)
    ((syntax-case expr (keywords k ...) (when pattern x xs ...) ...)
      (%syntax-case expr (k ...)
        (pattern x xs ...) ...))
    ((syntax-case expr . x)
      (syntax-case expr (keywords) . x)))
)
