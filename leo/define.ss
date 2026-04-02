(library (leo define)
  (export define value record union type)
  (import
    (rename
      (except (scheme) lambda)
      (define %define))
    (rename (data) (data %data))
    (rename (union) (union %union))
    (leo lambda)
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes))

  (define-keywords value record union type)

  (define-rules-syntaxes
    (keywords value lambda syntax and when keywords record type union)

    ((define-1 (value (id x)))
      (%define id x))

    ((define-1 (lambda (id x xs ...)))
      (%define (id) x xs ...))

    ((define-1 (lambda (id param ... (and last)) x xs ...))
      (%define (id param ... . last) x xs ...))

    ((define-1 (lambda (id param ...) x xs ...))
      (%define (id param ...) x xs ...))

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

    ((define-1 (record (type . x)))
      (define-record-type . x))

    ((define-1 (record . x))
      (%data . x))

    ((define-1 (union . x))
      (%union . x))

    ((define-1 (id x))
      (keyword? id)
      (define-1 (value (id x))))

    ((define x ...)
      (begin
        (define-1 x)
        ...)))
)
