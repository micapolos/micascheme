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

(define true-term selector)
(define false-term (rejector selector))

(define (if-term $condition $consequent $alternate)
  (match-term $condition
    (application
      (lambda (_) $alternate)
      (lambda (_) $consequent))))

(define (cons-term $car $cdr)
  (application (application selector $car) $cdr))

(define (match-term $pair $body)
  (application (application matcher $pair) $body))

(define (car-term $pair)
  (match-term $pair
    (lambda ($cdr)     ;; Outer: "bar" is peeled first
      (lambda ($car)   ;; Inner: "foo" is peeled second
        $car))))       ;; Return the inner one for 'car'

(define (cdr-term $pair)
  (match-term $pair
    (lambda ($cdr)     ;; Outer: "bar"
      (lambda ($car)   ;; Inner: "foo"
        $cdr))))       ;; Return the outer one for 'cdr'

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

(check-term-datum=?
  (normalize (cdr-term (cons-term (native "foo") (native "bar"))))
  (native "bar"))

(check-term-datum=?
  (normalize (if-term true-term (native "true") (native "false")))
  (native "false"))

(check-term-datum=?
  (normalize (if-term false-term (native "true") (native "false")))
  (native "false"))
