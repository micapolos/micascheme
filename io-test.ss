(import (micascheme) (io) (monad))

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

(check
  (equal?
    (io-unsafe-run
      (monad-lets io-monad
        ($var1 (io-var "foo"))
        ($var2 (io-var "bar"))
        ($value1 (io-get $var1))
        ($value2 (io-get $var2))
        (_ (io-set $var1 (string-append $value1 "+")))
        ($value1 (io-get $var1))
        (_ (io-set $var2 (string-append $value1 $value2)))
        (io-get $var2)))
    "foo+bar"))
