(library (leo define-syntax)
  (export
    rules
    define-syntax)
  (import
    (rename (scheme)
      (define-syntax %define-syntax)
      (case %case))
    (syntax-keywords)
    (keyword)
    (syntax)
    (leo case)
    (rename
      (syntaxes)
      (define-rules-syntax %define-rules-syntax)
      (define-rules-syntaxes %define-rules-syntaxes)))

  (define-keywords rules)

  (%define-rules-syntaxes
    (keywords rules case when keywords)

    ((define-syntax (keywords k ...) (rules (when pattern x xs ...) ...))
      (%define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-syntax (keywords k ...) (case (when pattern x xs ...) ...))
      (define-case-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-syntax (rules (when pattern x xs ...) ...))
      (define-syntax (keywords) (rules (when pattern x xs ...) ...)))

    ((define-syntax (case (when pattern x xs ...) ...))
      (define-syntax (keywords) (case (when pattern x xs ...) ...)))

    ((define-syntax (id x))
      (keyword? id)
      (%define-syntax id x))

    ((define-syntax (id s) x xs ...)
      (keyword? id)
      (%define-syntax (id s) x xs ...)))
)
