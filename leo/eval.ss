(library (leo eval)
  (export eval)
  (import
    (rename (scheme) (eval %eval))
    (leo expand))

  (define eval
    (case-lambda
      ((x)
        (eval x (interaction-environment)))
      ((x env)
        (%eval
          (cons leo-expand x)
          env))))
)
