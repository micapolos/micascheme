(import (scheme) (check) (finalize) (lets) (procedure))

(define finalized (list))

(define-finalize (+ $sum)
  (set! finalized (cons `(+ ,$sum) finalized)))

(check (equal? finalized '()))

(lets
  ($sum1 (+ 2 2))
  ($sum2 (+ 5 7))
  (run
    (check (equal? $sum1 4))
    (check (equal? $sum2 12))
    (check (equal? finalized '()))))

(check (equal? finalized '((+ 4) (+ 12))))
