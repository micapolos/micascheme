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

(define (cons-term $car $cdr)
  (application (application selector $car) $cdr))

(define (pair-switch-term $pair $body)
  (application (application matcher $pair) $body))

(define (car-term $pair)
  (pair-switch-term $pair
    (lambda ($car)
      (lambda ($cdr) $car))))

(define (cdr-term $pair)
  (pair-switch-term $pair
    (lambda ($car)
      (lambda ($cdr) $cdr))))

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
  (normalize
    (branch
      (native #t)
      (native "true")
      (unreachable "false")))
  (native "true"))

(check-term-datum=?
  (normalize
    (branch
      (native #f)
      (unreachable "true")
      (native "false")))
  (native "false"))

(check-term-datum=?
  (normalize
    (branch
      (variable 0)
      (application (native number->string) (native 10))
      (application (native number->string) (native 20))))
  (branch
    (variable 0)
    (native "10")
    (native "20")))

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fn)
          (lambda ($n)
            (branch (application (native zero?) $n)
              (native "OK")
              (application $fn
                (application-2 (native curry-) $n (native 1)))))))
      (native 5)))
  (native "OK"))

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fib)
          (lambda ($n)
            (branch (application-2 (native curry<) $n (native 2))
              $n
              (application-2 (native curry+)
                (application $fib (application-2 (native curry-) $n (native 1)))
                (application $fib (application-2 (native curry-) $n (native 2))))))))
      (native 10)))
  (native 55))

(check-term-datum=?
  (normalize (car-term (cons-term (native "foo") (native "bar"))))
  (native "foo"))
