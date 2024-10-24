(library (current)
  (export
    unsafe-current
    unsafe-current-get

    current
    current-bind)
  (import
    (scheme)
    (lets)
    (monad)
    (syntax))

  (define-rule-syntax (unsafe-current $body ...)
    (lambda () $body ...))

  (define (unsafe-current-get $current)
    ($current))

  (define-monad current
    ((pure $value)
      (unsafe-current $value))
    ((bind $current $fn)
      (unsafe-current
        (unsafe-current-get
          ($fn
            (unsafe-current-get $current))))))
)
