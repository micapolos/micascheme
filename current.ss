(library (current)
  (export
    unsafe-current
    unsafe-current-get

    current
    current-bind

    current-variable
    set-current
    get-current

    current-time-seconds
    current-random
    current-random-below
    current-random-seed
    set-current-random-seed)
  (import
    (micascheme))

  (define-syntax-rule (unsafe-current $body ...)
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

  (define current-time-seconds
    (unsafe-current (current-time)))

  (define (current-random-below $number)
    (unsafe-current
      (random $number)))

  (define (current-random)
    (current-random-below 1.0))

  (define (current-random-seed)
    (unsafe-current
      (random-seed)))

  (define (set-current-random-seed $seed)
    (unsafe-current
      (random-seed $seed)
      (void)))
)
