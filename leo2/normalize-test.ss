(import
  (leo2 base)
  (leo2 term)
  (leo2 normalize)
  (leo2 datum))

(define (zero?-term $term)
  (switch $term
    ((number? $number)
      (zero? $number))
    ((else $other)
      (neutral (application zero? $other)))))

(check-term-datum=?
  (normalize (application zero?-term 0))
  #t)

(check-term-datum=?
  (normalize (application zero?-term 123))
  #f)
