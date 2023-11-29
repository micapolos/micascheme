(library (io)
  (export
    io io-bind
    io-var
    io-get
    io-set)
  (import (micascheme))

  (define (io-run $io) ($io))

  (define-monad io
    ((pure $value)
      (lambda () $value))
    ((bind $value $fn)
      (lambda ()
        (app ($fn (app $value))))))

  (define (io-var $value)
    (lambda () (box $value)))

  (define (io-set $var $value)
    (lambda () (set-box! $var $value)))

  (define (io-get $var)
    (lambda () (unbox $var)))
)
