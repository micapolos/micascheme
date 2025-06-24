(library (typico lang)
  (export typico check-equal? check-raises)
  (import
    (only (micascheme)
      define-syntax define-rule-syntax define-case-syntax syntax-case
      syntax datum->syntax
      eval environment
      quote check equal? raises)
    (typico typed)
    (typico expand)
    (typico core-lookup))

  (define-syntax (typico $syntax)
    (syntax-case $syntax ()
      ((id expr)
        (datum->syntax #'id
          (eval
            (typed-value (expand-typed (core-lookup) #'expr))
            (environment '(micascheme)))))))

  (define-rule-syntax (check-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-case-syntax (check-raises in)
    (check (raises (expand-typed (core-lookup) #'in))))
)
