(library (leo2 equal)
  (export term=? check-term=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define (term=? $depth $term-a $term-b)
    (equal?
      (term->datum $depth #t $term-a)
      (term->datum $depth #t $term-b)))

  (define-rule-syntax (check-term=? in out)
    (check
      (equal?
        (term->datum 0 #t in)
        (term->datum 0 #t out))))
)
