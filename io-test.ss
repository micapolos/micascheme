(import (scheme) (check) (io))

(check (equal? (io-run (io 128)) 128))

(define io-counter
  (let ()
    (define $counter 0)
    (unsafe-io
      (set! $counter (+ $counter 1))
      $counter)))

(define (io-counter-items $count)
  (unsafe-io
    (reverse
      (fold-left
        (lambda ($list _)
          (cons (io-run io-counter) $list))
        (list)
        (iota $count)))))

(check (equal? (io-run io-counter) 1))
(check (equal? (io-run io-counter) 2))
(check (equal? (io-run io-counter) 3))

; 4 items: 5 6 7 8
(check
  (equal?
    (io-run
      (io-bind io-counter
        (lambda ($count)
          (io-counter-items $count))))
    (list 5 6 7 8)))

; 9 items: 10 11 12 13 14 15 16 17 18
(check
  (equal?
    (io-run
      (io-bind io-counter
        (lambda ($count)
          (io-counter-items $count))))
    (list 10 11 12 13 14 15 16 17 18)))
