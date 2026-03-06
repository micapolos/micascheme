(library (leo2 apply)
  (export term-apply)
  (import
    (leo2 base)
    (leo2 term))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((lambda? $lambda)
        (lambda-apply $lambda $rhs))
      ((lambda-type? $lambda-type)
        (lambda-type-apply $lambda-type $rhs))
      ((recursion? $recursion)
        (term-apply (recursion-apply $recursion $lhs) $rhs))
      ((else $other)
        (neutral (application $lhs $rhs)))))
)
