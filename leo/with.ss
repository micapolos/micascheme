(library (leo with)
  (export with implicit)
  (import
    (scheme)
    (syntaxes)
    (syntax-keywords))

  (define-rules-syntax
    (keywords implicit)
    ((with (implicit template-id ids ...) x xs ...)
      (with-implicit (template-id ids ...) x xs ...))
    ((with x ...)
      (x ...)))
)
