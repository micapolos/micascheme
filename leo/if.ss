(library (leo if)
  (export if then)
  (import
    (rename (scheme) (if %if))
    (syntax-keywords)
    (syntax)
    (syntaxes))

  (define-keywords then)

  (define-rules-syntax
    (keywords then else)

    ((if a (then b bs ...) (else c cs ...))
      (%if a (begin b bs ...) (begin c cs ...)))

    ((if a b c)
      (%if a b c)))
)
