(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum))

(define lambda-zero?
  (native-lambda zero? number?))

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
