(library (io)
  (export
    unsafe-io
    io-unsafe-run
    (rename (pure-io io))
    io-bind

    io-monad

    io-var
    io-get
    io-set)
  (import (micascheme) (monad))

  (data (io proc))

  (define-syntax-rule (unsafe-io $body ...)
    (io (lambda () $body ...)))

  (define (io-unsafe-run $io)
    ((io-proc $io)))

  (define (pure-io $value)
    (unsafe-io $value))

  (define (io-bind $io $fn)
    (unsafe-io (io-unsafe-run ($fn (io-unsafe-run $io)))))

  (define io-monad
    (monad pure-io io-bind))

  (define (io-var $value)
    (unsafe-io (box $value)))

  (define (io-set $var $value)
    (unsafe-io (set-box! $var $value)))

  (define (io-get $var)
    (unsafe-io (unbox $var)))
)
