(library (typico lang)
  (export typico check-equal? check-primitive check-raises)
  (import
    (only (micascheme)
      define-rule-syntax define-case-syntax
      syntax
      check equal? raises $primitive)
    (typico typed)
    (typico expand)
    (typico eval)
    (typico core lookup))

  (define-rule-syntax (typico x)
    (typed-eval (expand-typed (core-lookup) #'x)))

  (define-rule-syntax (check-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-rule-syntax (check-primitive in out)
    (check (equal? (typico in) ($primitive 3 out))))

  (define-case-syntax (check-raises in)
    (check (raises (typico in))))
)
