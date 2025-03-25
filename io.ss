(library (io)
  (export
    unsafe-io
    io
    io-run
    io-bind)
  (import (scheme) (syntax))

  (define-rule-syntax (unsafe-io body ...)
    (lambda () body ...))

  (define (io $value)
    (unsafe-io $value))

  (define (io-run $io)
    ($io))

  (define (io-bind $io $fn)
    (unsafe-io (($fn ($io)))))
)
