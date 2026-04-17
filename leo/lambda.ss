(library (leo lambda)
  (export lambda case-lambda)
  (import
    (rename (scheme)
      (lambda %lambda)
      (case-lambda %case-lambda))
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes)
    (leo with))

  (define-rules-syntaxes
    (keywords when with and)

    ((lambda (with param ... (and last)) x xs ...)
      (%lambda (param ... . last) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...))

    ((case-lambda (when (with ps ...) x xs ...) ...)
      (%case-lambda ((ps ...) x xs ...) ...)))
)
