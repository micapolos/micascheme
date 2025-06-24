(library (typico lang)
  (export typico check-equal? check-raises)
  (import
    (only (micascheme) define-syntax syntax-case eval syntax environment quote define-rule-syntax check equal?)
    (typico typed)
    (typico expand)
    (typico lookup))

  (define-syntax (typico $syntax)
    (syntax-case $syntax ()
      ((_ expr)
        (eval
          (typed-value (expand-typed (core-lookup) #'expr))
          (environment '(micascheme))))))

  (define-rule-syntax (check-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-rule-syntax (check-raises in)
    (check (raises (typico in))))
)
