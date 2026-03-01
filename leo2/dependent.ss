(library (leo2 dependent)
  (export
    lambda-dependent?
    lambda-type-dependent?
    recursion-dependent?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal))

  (define (lambda-dependent? $lambda)
    (not
      (term=?
        ($lambda (variable 0))
        ($lambda (variable 1)))))

  (define (lambda-type-dependent? $lambda-type)
    (lambda-dependent?
      (lambda-type-lambda $lambda-type)))

  (define (recursion-dependent? $recursion)
    (lambda-dependent?
      (recursion-lambda $recursion)))
)
