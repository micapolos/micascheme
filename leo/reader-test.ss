(import (micascheme) (leo reader))

(check
  (equal?
    (reader-eval (list-reader)
      12
      negate
      (add 13)
      (add
        5
        negate
        (subtract 3)))
    `(
      12
      negate
      (add 13)
      (add
        5
        negate
        (subtract 3)))))

(define calculator-reader
  (case-lambda
    (()
      (calculator-reader identity))
    (($end-fn)
      (calculator-reader #f $end-fn))
    (($value $end-fn)
      (reader
        (lambda ($append-value)
          (calculator-reader $append-value $end-fn))
        (lambda ($begin-symbol)
          (calculator-reader
            (lambda ($end-value)
              (case $begin-symbol
                ((one) (calculator-reader 1 $end-fn))
                ((add) (calculator-reader (+ $value $end-value) $end-fn))
                ((negate) (calculator-reader (- $value) $end-fn))))))
        (lambda ()
          ($end-fn $value))))))

(check
  (equal?
    (reader-eval (calculator-reader)
      10
      (add one)
      (add 100 (add 200))
      negate)
    -311))

(check
  (equal?
    (reader-eval (datums-reader identity) 1 2 (x 3 4))
    `(1 2 (x 3 4))))
