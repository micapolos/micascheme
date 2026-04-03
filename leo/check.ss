(library (leo check)
  (export check raises works)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (rename (check)
      (check %check)
      (raises %raises)
      (works %works)))

  (define-keywords raises works)

  (define-rules-syntaxes
    (keywords raises works)

    ((check (raises . x))
      (check (%raises . x)))

    ((check (works . x))
      (check (%works . x)))

    ((check . x)
      ; TODO: Remove exception handler when check raises readable leo exceptions
      (with-exception-handler
        (base-exception-handler)
        (lambda () (%check . x)))))
)
