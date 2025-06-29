(library (typico eval)
  (export typed-eval)
  (import
    (micascheme)
    (typico expand)
    (typico typed)
    (typico type)
    (typico core types)
    (typico environment))

  (define (typed-eval (typed $type $value))
    (eval $value (typico-environment)))
)
