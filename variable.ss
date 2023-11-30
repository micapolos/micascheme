(library (variable)
  (export
    current-variable
    get-current
    set-current)
  (import
    (scheme)
    (current)
    (procedure))

  (define (current-variable $value)
    (unsafe-current (box $value)))

  (define (get-current $variable)
    (unsafe-current
      (unbox $variable)))

  (define (set-current $variable $value)
    (unsafe-current
      (run (set-box! $variable $value))))
)
