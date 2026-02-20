(library (leo2 equal)
  (export term=? check-term=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define (term=? $depth $term-a $term-b)
    (equal?
      (term->datum #t #t $depth $term-a)
      (term->datum #t #t $depth $term-b)))

  (define-rule-syntax (check-term=? in out)
    (check
      (equal?
        (term->datum #t #t 0 in)
        (term->datum #t #t 0 out))))
)
