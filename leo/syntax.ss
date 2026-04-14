(library
  (leo syntax)
  (export match with-syntax with-identifier)
  (import
    (rename (scheme)
      (with-syntax %with-syntax)
      (with-implicit %with-implicit))
    (leo in)
    (keyword)
    (syntax-keywords)
    (syntax)
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

  (define-keyword match)

  (define-rules-syntaxes
    (keywords match in)
    ((with-syntax (match pattern expr) ... (in x xs ...))
      (%with-syntax ((pattern expr) ...) x xs ...))
    ((with-identifier (id expr) ... (in x xs ...))
      (for-all identifier? #'(id ...))
      (%with-syntax ((id expr) ...) x xs ...)))
)
