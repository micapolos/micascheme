(library (typico lang)
  (export typico check-equal? check-primitive check-raises print typico-expand)
  (import
    (only (micascheme)
      define-rule-syntax define-case-syntax
      define apply sc-expand environment quote
      syntax
      check equal? raises $primitive displayln)
    (typico typed)
    (typico expand)
    (typico eval)
    (typico core lookup)
    (typico environment))

  (define-rule-syntax (print x)
    (displayln (typed-eval (expand-typed (core-lookup) #'x))))

  (define-rule-syntax (typico x)
    (typed-eval (expand-typed (core-lookup) #'x)))

  (define-rule-syntax (check-equal? in out)
    (check (equal? (typico in) (typico out))))

  (define-rule-syntax (check-primitive in out)
    (check (equal? (typico in) ($primitive 3 out))))

  (define-case-syntax (check-raises in)
    (check (raises (typico in))))

  (define (typico-expand $syntax $environment . $options)
    (apply sc-expand
      (expand-typed (core-lookup) $syntax)
      (typico-environment)
      $options))
)
