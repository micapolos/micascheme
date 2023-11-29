(library (variable)
  (export
    current-variable
    get-current
    set-current)
  (import
    (scheme)
    (current)
    (lets))

  (define (current-variable $value)
    (unsafe-current (box $value)))

  (define (get-current $variable)
    (unsafe-current
      (unbox $variable)))

  (define (set-current $variable $value)
    (unsafe-current
      (lets
        (do (set-box! $variable $value))
        (void))))
)
