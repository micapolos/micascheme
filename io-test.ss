(import (micascheme) (io))

(check (equal? (io-unsafe-run (unsafe-io 128)) 128))

(define $current 0)

(define (io-counter)
  (unsafe-io
    (define $value (+ $current 1))
    (set! $current $value)
    $value))

(check (equal? (io-unsafe-run (io-bind (io-counter) (lambda ($value) (io (+ $value 100))))) 101))
(check (equal? (io-unsafe-run (io-bind (io-counter) (lambda ($value) (io (+ $value 100))))) 102))
(check (equal? (io-unsafe-run (io-bind (io-counter) (lambda ($value) (io (+ $value 100))))) 103))
