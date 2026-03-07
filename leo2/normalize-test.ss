(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum)
  (curry))

(define (unreachable $value)
  (application (native raise) (native $value)))

(define (application-2 $fn $lhs $rhs)
  (application (application $fn $lhs) $rhs))

(define null-term
  (lambda ($0) $0))

(define true-term
  (lambda ($0) (lambda ($1) $0)))

(define false-term
  (lambda ($0) (lambda ($1) $1)))

(define (boolean-term $term)
  (switch $term
    ((native? $native)
      (if (native-ref $native) true-term false-term))
    ((else $other)
      (application boolean-term $term))))

(define (pair-term $car $cdr)
  (lambda ($boolean)
    (application
      (application $boolean $car)
      $cdr)))

(define (car-term $pair)
  (application $pair true-term))

(define (cdr-term $pair)
  (application $pair false-term))

(define (branch-term $condition $consequent $alternate)
  (application
    (application
      (pair-term
        (lambda (_) $consequent)
        (lambda (_) $alternate))
      $condition)
    null-term))

(define (recursive-term $f)
  (application
    (lambda ($x)
      (application $f
        (lambda ($v)
          (application (application $x $x) $v))))
    (lambda ($x)
      (application $f
        (lambda ($v)
          (application (application $x $x) $v))))))

(check-term-datum=?
  (normalize (native 123))
  (native 123))

(check-term-datum=?
  (normalize (application (native number->string) (native 123)))
  (native "123"))

(check-term-datum=?
  (normalize (application-2 (native curry-) (native 30) (native 20)))
  (native 10))

(check-term-datum=?
  (normalize (car-term (pair-term (native "car") (native "cdr"))))
  (native "car"))

(check-term-datum=?
  (normalize (cdr-term (pair-term (native "car") (native "cdr"))))
  (native "cdr"))

(check-term-datum=?
  (normalize
    (branch-term
      true-term
      (native "true")
      (unreachable "false")))
  (native "true"))

(check-term-datum=?
  (normalize
    (branch-term
      false-term
      (unreachable "true")
      (native "false")))
  (native "false"))

(check-term-datum=?
  (normalize
    (application
      (recursive-term
        (lambda ($fn)
          (lambda ($n)
            (branch-term (application boolean-term (application (native zero?) $n))
              (native "OK")
              (application $fn
                (application-2 (native curry-) $n (native 1)))))))
      (native 5)))
  (native "OK"))

(check-term-datum=?
  (normalize
    (application
      (recursive-term
        (lambda ($fib)
          (lambda ($n)
            (branch-term (application boolean-term (application-2 (native curry<) $n (native 2)))
              $n
              (application-2 (native curry+)
                (application $fib (application-2 (native curry-) $n (native 1)))
                (application $fib (application-2 (native curry-) $n (native 2))))))))
      (native 10)))
  (native 55))
