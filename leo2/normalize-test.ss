(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum))

(define lambda-zero?
  (native-lambda zero? number?))

(check-term-datum=?
  (normalize (application lambda-zero? 0))
  #t)

(check-term-datum=?
  (normalize (application lambda-zero? 123))
  #f)
