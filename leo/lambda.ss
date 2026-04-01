(library (leo lambda)
  (export lambda)
  (import
    (rename (scheme) (lambda %lambda))
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes)
    (leo with))

  (define-rules-syntaxes
    (keywords with and)

    ((lambda (with param ... (and last)) x xs ...)
      (%lambda (param ... . last) x xs ...))
    ((lambda (with param ...) x xs ...)
      (%lambda (param ...) x xs ...))
    ((lambda x xs ...)
      (%lambda () x xs ...)))
)
