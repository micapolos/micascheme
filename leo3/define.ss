(library (leo3 define)
  (export define)
  (import
    (rename (scheme) (define %define))
    (syntax)
    (syntaxes)
    (keyword))

  (define-rules-syntax
    ((define id)
      (keyword? id)
      (%define id))
    ((define (id x))
      (keyword? id)
      (%define id x))
    ((define (id param ...) x xs ...)
      (and
        (for-all identifier? #'(id param ...)))
      (%define
        (id param ...)
        x xs ...))
    ((define (id param ... (andd last-param)) x xs ...)
      (and
        (for-all identifier? #'(id param ... last-param))
        (equal? (datum andd) 'and))
      (%define
        (id param ... . last-param)
        x xs ...)))
)
