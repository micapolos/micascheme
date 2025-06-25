(library (typico eval)
  (export typed-eval)
  (import
    (micascheme)
    (typico expand)
    (typico typed)
    (typico type)
    (typico core types)
    (typico environment)
    (typico scoped))

  (define (typed-eval (typed $type $value))
    (eval
      (cond
        ((type=? $type syntax-type) `(syntax ,(scoped-ref $value)))
        (else $value))
      (typico-environment)))
)
