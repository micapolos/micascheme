(library (leo2 reduce)
  (export reduce)
  (import
    (leo2 base)
    (leo2 term))

  (define (reduce $lhs $rhs)
    (switch $lhs
      ((lambda? $lambda)
        (lambda-apply $lambda $rhs))
      ((lambda-type? $lambda-type)
        (lambda-type-apply $lambda-type $rhs))
      ((recursion? $recursion)
        (reduce (recursion-apply $recursion $lhs) $rhs))
      ((else $other)
        (neutral (application $lhs $rhs)))))
)
