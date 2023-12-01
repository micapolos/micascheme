(library (number)
  (export
    pi pi2
    fract
    iterate
    nonnegative-integer?
    one?)
  (import (scheme))

  (define pi (* (asin 1) 2))
  (define pi2 (* (asin 1) 4))

  (define (fract $number)
    (- $number (floor $number)))

  (define (nonnegative-integer? $obj)
    (and (integer? $obj) (nonnegative? $obj)))

  (define (iterate $proc $item $count)
    (cond
      ((= $count 0) $item)
      (else (iterate $proc ($proc $item) (- $count 1)))))

  (define (one? $number)
    (= $number 1))
)
