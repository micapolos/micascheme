(library (leo2 evaluator)
  (export)
  (import
    (leo2 base)
    (leo2 term))

  (define (evaluate $term)
    (evaluated
      (switch $term
        ((variable? $variable)
          $variable)
        ((lambda? $lambda)
          (evaluated
            (lambda ($0)
              (evaluate (lambda-apply $lambda $0)))))
        ((application? $application)
          (term-apply
            (evaluate (application-lhs $application))
            (evaluate (application-rhs $application)))))))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((lambda? $lambda)
        (lambda-apply $lambda $rhs))
      ((lambda-type? $lambda-type)
        (lambda-type-apply $lambda-type $rhs))
      ((recursion? $recursion)
        (term-apply (recursion-apply $recursion $lhs) $rhs))
      ((else $other)
        (application $lhs $rhs))))
)
