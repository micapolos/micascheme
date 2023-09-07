(import (micascheme) (leo reader))

(define calculator-reader
  (case-lambda
    (() (calculator-reader #f (lambda (_) (throw end))))
    (($value $end-fn)
      (switch $value
        ((false? _)
          (reader #f
            (lambda ($appended-value)
              (calculator-reader $appended-value $end-fn))
            (lambda ($begin-symbol)
              (case $begin-symbol
                ((pi)
                  (calculator-reader #f
                    (lambda ($ended-value)
                      (switch $ended-value
                        ((false? _) (calculator-reader 3.14159 $end-fn))
                        ((else $other) (calculator-reader `(pi ,$other) $end-fn))))))
                (else
                  (calculator-reader #f
                    (lambda ($ended-value)
                      (calculator-reader `(,$begin-symbol ,$ended-value) $end-fn))))))
            $end-fn))
        ((number? $number)
          (reader $number
            (lambda ($appended-value)
              (calculator-reader `(,$number ,$appended-value) $end-fn))
            (lambda ($begin-symbol)
              (case $begin-symbol
                ((add)
                  (calculator-reader #f
                    (lambda ($ended-value)
                      (switch $ended-value
                        ((number? $ended-number) (calculator-reader (+ $number $ended-number) $end-fn))
                        ((else $other) (calculator-reader `($number (,$begin-symbol ,$other)) $end-fn))))))
                ((negate)
                  (calculator-reader #f
                    (lambda ($ended-value)
                      (switch $ended-value
                        ((false? _) (calculator-reader (- $number) $end-fn))
                        ((else $other) (calculator-reader `($number (,$begin-symbol ,$other)) $end-fn))))))
                (else
                  (calculator-reader #f
                    (lambda ($ended-value)
                      (calculator-reader `($number (,$begin-symbol ,$ended-value)) $end-fn))))))
            $end-fn))
        ((else $other)
          (throw calculator $other))))))

(check
  (equal?
    (reader-eval (calculator-reader)
      pi
      (add pi)
      negate
      (add 100)
      negate)
    -93.71682))
