(library
  (leo syntax)
  (export with-syntax)
  (import
    (rename (scheme)
      (with-syntax %with-syntax)
      (with-implicit %with-implicit))
    (leo with)
    (keyword)
    (syntax-keywords)
    (syntaxes))
  (export
    (import
      (only (scheme)
        syntax
        quasisyntax
        unsyntax
        unsyntax-splicing
        with-implicit)
      (rename
        (only (syntax)
          syntax?
          syntax=?
          define-keyword
          define-keywords)
        (syntax=? free-syntax=?))))

  (define-rules-syntaxes
    (keywords with)
    ((with-syntax (with (pattern expr) ...) x xs ...)
      (%with-syntax ((pattern expr) ...) x xs ...)))
)
