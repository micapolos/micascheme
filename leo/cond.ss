(library (leo cond)
  (export cond)
  (import
    (rename (scheme) (cond %cond))
    (syntax-keywords)
    (syntaxes))

  (define-rules-syntax
    (keywords when else)

    ((cond (when a b bs ...) ... (else c cs ...))
      (%cond (a b bs ...) ... (else c cs ...)))

    ((cond (when a b bs ...) ...)
      (%cond (a b bs ...) ...)))
)
