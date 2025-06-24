(library (typico lang)
  (export typico check-equal? check-primitive check-raises)
  (import
    (only (micascheme)
      define-syntax define-rule-syntax define-case-syntax syntax-case
      syntax quasisyntax datum->syntax
      eval environment
      quote check equal? raises $primitive logging)
    (typico typed)
    (typico expand)
    (typico core lookup))

  (define-syntax (typico $syntax)
    (syntax-case $syntax ()
      ((id expr)
        #`(eval
          (typed-value (expand-typed (core-lookup) #'expr))
          (environment '(micascheme))))))

  (define-rule-syntax (check-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-rule-syntax (check-primitive in out)
    (check (equal? (typico in) ($primitive 3 out))))

  (define-case-syntax (check-raises in)
    (check (raises (typico in))))
)
