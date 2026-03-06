(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum))

(define lambda-zero?
  (native-lambda zero? number?))

(define (lambda- $lhs)
  (switch $lhs
    ((number? $lhs-number)
      (lambda ($rhs)
        (switch $rhs
          ((number? $rhs-number)
            (- $lhs-number $rhs-number))
          ((else $rhs-other)
            (application (native `(- ,$lhs-number)) $rhs-other)))))
    ((else $lhs-other)
      (application '- $lhs-other))))

(define (dec $number) (- $number 1))

(define lambda-dec
  (native-lambda dec number?))

(check-term-datum=?
  (normalize (application lambda-zero? 0))
  #t)

(check-term-datum=?
  (normalize (application lambda-zero? 123))
  #f)

(check-term-datum=?
  (normalize (application lambda-dec 10))
  9)

(check-term-datum=?
  (normalize (application (application lambda- 30) 20))
  10)

(check-term-datum=?
  (normalize (application (application lambda- (variable 0)) (variable 1)))
  (neutral (application (neutral (application '- (variable 0))) (variable 1))))

(check-term-datum=?
  (normalize (application lambda- 30))
  (lambda ($0) (neutral (application (native '(- 30)) $0))))

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fn)
          (lambda ($n)
            (branch (application zero? $n)
              "OK"
              (application $fn (application lambda-dec $n))))))
      10))
  "OK")

(check-term-datum=?
  (normalize
    (application
      (recursion
        (lambda ($fn)
          (lambda ($n)
            (branch (application zero? $n)
              "OK"
              (application $fn (application lambda-dec $n))))))
      0))
  "OK")
