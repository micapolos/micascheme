(library (leo with)
  (export with)
  (import
    (scheme)
    (syntaxes)
    (syntax-keywords))

  (define-rules-syntax
    ((with x ...)
      (x ...)))
)
