(library (io)
  (export
    unsafe-io
    io-unsafe-run
    (rename (pure-io io))
    io-bind)
  (import (micascheme))

  (data (io proc))

  (define-syntax-rule (unsafe-io $body ...)
    (io (lambda () $body ...)))

  (define (io-unsafe-run $io)
    ((io-proc $io)))

  (define (pure-io $value)
    (unsafe-io $value))

  (define (io-bind $io $fn)
    (unsafe-io (io-unsafe-run ($fn (io-unsafe-run $io)))))
)
