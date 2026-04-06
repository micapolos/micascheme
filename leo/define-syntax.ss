(library (leo define-syntax)
  (export define-syntax)
  (import
    (rename (scheme)
      (define-syntax %define-syntax))
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes))

  (define-rules-syntaxes
    (keywords when keywords)

    ((define-syntax (keywords k ...) (when pattern x xs ...) ...)
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-syntax (when pattern x xs ...) ...)
      (define-syntax (keywords) (when pattern x xs ...) ...))

    ((define-syntax (id x))
      (keyword? id)
      (%define-syntax id x))

    ((define-syntax (id s) x xs ...)
      (keyword? id)
      (%define-syntax (id s) x xs ...)))
)
