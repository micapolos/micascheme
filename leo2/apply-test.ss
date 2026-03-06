(import
  (leo2 base)
  (leo2 term)
  (leo2 datum)
  (leo2 apply))

(define lambda-zero?
  (native-lambda zero? number?))

(check-term-datum=?
  (term-apply lambda-zero? 123)
  #f)

(check-term-datum=?
  (term-apply (native zero?) 123)
  (neutral (application (native zero?) 123)))
