(library (leo reader)
  (export
    reader reader? reader-value reader-append-fn reader-begin-fn reader-end-fn
    reader-append reader-begin reader-end
    reader-read reader-read-list
    calculator-reader)
  (import (micascheme))

  (data (reader value append-fn begin-fn end-fn))

  (define (reader-append $reader $value)
    ((reader-append-fn $reader) $value))

  (define (reader-begin $reader $symbol)
    ((reader-begin-fn $reader) $symbol))

  (define (reader-end $reader)
    ((reader-end-fn $reader) (reader-value $reader)))

  (define (reader-read-list $reader $list)
    (fold-left reader-read $reader $list))

  (define (reader-read $reader $datum)
    (switch $datum
      ((symbol? $symbol)
        (reader-end
          (reader-begin $reader $symbol)))
      ((pair? $pair)
        (reader-end
          (reader-read-list
            (reader-begin $reader (car $pair))
            (cdr $pair))))
      ((else $other)
        (reader-append $reader $other))))

  (define (calculator-reader $value $end-fn)
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
        (throw calculator $other))))
)
