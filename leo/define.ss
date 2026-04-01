(library (leo define)
  (export define value)
  (import
    (rename
      (except (scheme) lambda)
      (define %define))
    (leo lambda)
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes))

  (define-keywords value)

  (define-rules-syntaxes
    (keywords value lambda syntax and when keywords)

    ((define-1 (value (id x)))
      (%define id x))

    ((define-1 (lambda (name param ... (and last)) x xs ...))
      (%define (name param ... . last) x xs ...))

    ((define-1 (lambda (name param ...) x xs ...))
      (%define (name param ...) x xs ...))

    ((define-1 (syntax (keywords k ...) (when pattern x xs ...) ...))
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-1 (syntax (when pattern x xs ...) ...))
      (define (syntax (keywords) (when pattern x xs ...) ...)))

    ((define-1 (syntax (id x)))
      (keyword? id)
      (define-syntax id x))

    ((define-1 (syntax (id s) x xs ...))
      (keyword? id)
      (define-syntax (id s) x xs ...))

    ((define-1 (id x))
      (keyword? id)
      (define-1 (value (id x))))

    ((define x ...)
      (begin
        (define-1 x)
        ...)))
)
