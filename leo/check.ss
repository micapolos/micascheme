(library (leo check)
  (export check raises works)
  (import
    (scheme)
    (syntaxes)
    (rename (check)
      (check %check)
      (raises %raises)
      (works %works)))

  (define-rules-syntaxes
    ((check . x)
      ; TODO: Remove exception handler when check raises readable leo exceptions
      (with-exception-handler
        (base-exception-handler)
        (lambda () (%check . x))))
    ((raises . x)
      (%raises . x))
    ((works . x)
      (%works . x)))
)
