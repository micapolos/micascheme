(import (scheme) (check) (define))

(define-recursive (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(check (equal? (fib 10) 55))
