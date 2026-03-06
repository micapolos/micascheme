(import
  (leo2 base)
  (leo2 term)
  (leo2 datum)
  (leo2 apply))

(define (zero?-term $term)
  (switch $term
    ((number? $number)
      (zero? $number))
    ((else $other)
      (neutral (application zero?-term $term)))))

(check-term-datum=?
  (term-apply zero?-term 123)
  #f)
