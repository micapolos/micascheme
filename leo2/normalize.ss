(library (leo2 normalize)
  (export
    normalize
    reduce)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 reduce))

  (define (normalize $term)
    (switch-exhaustive $term
      ((neutral? $neutral) $neutral)
      ((primitive? $primitive) $primitive)
      ((variable? $variable) $variable)
      ((lambda? $lambda)
        (lambda ($0)
          (normalize (lambda-apply $lambda $0))))
      ((application? $application)
        (reduce
          (normalize (application-lhs $application))
          (normalize (application-rhs $application))))))
)
