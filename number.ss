(library (number)
  (export
    pi pi2
    fract
    iterate
    iterate-indexed
    positive-integer?
    nonnegative-integer?
    one?
    bitwise-align)
  (import (scheme))

  (define pi (* (asin 1) 2))
  (define pi2 (* (asin 1) 4))

  (define (fract $number)
    (- $number (floor $number)))

  (define (nonnegative-integer? $obj)
    (and (integer? $obj) (nonnegative? $obj)))

  (define (positive-integer? $obj)
    (and (integer? $obj) (positive? $obj)))

  (define (iterate $proc $item $count)
    (cond
      ((= $count 0) $item)
      (else (iterate $proc ($proc $item) (- $count 1)))))

  (define (iterate-indexed $proc $item $count)
    (iterate-indexed-from $proc $item $count 0))

  (define (iterate-indexed-from $proc $item $count $index)
    (cond
      ((= $count 0) $item)
      (else
        (iterate-indexed-from
          $proc
          ($proc $item $index)
          (- $count 1)
          (add1 $index)))))

  (define (one? $number)
    (= $number 1))

  (define (bitwise-align $value $alignment)
    (let (($mask (- $alignment 1)))
      (bitwise-and
        (+ $value $mask)
        (bitwise-not $mask))))
)
