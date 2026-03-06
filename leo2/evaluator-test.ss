(import
  (leo2 base)
  (leo2 term)
  (leo2 evaluator)
  (leo2 datum))

(define (zero?-term $term)
  (switch $term
    ((number? $number)
      (zero? $number))
    ((else $other)
      (application (neutral zero?) $other))))

(check-term-datum=?
  (evaluate (application zero?-term 0))
  #t)

(check-term-datum=?
  (evaluate (application zero?-term 123))
  #f)
