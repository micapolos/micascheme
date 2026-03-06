(library (leo2 normalize)
  (export
    normalize)
  (import
    (leo2 base)
    (leo2 term))

  (define (normalize $term)
    (switch-exhaustive $term
      ((native? $native) $native)
      ((primitive? $primitive) $primitive)
      ((variable? $variable) $variable)
      ((lambda? $lambda)
        (lambda ($arg)
          (normalize (lambda-apply $lambda $arg))))
      ((lambda-type? $lambda-type)
        (lambda-type
          (normalize (lambda-type-param $lambda-type))
          (normalize (lambda-type-lambda $lambda-type))))
      ((recursion? $recursion)
        (recursion
          (normalize
            (recursion-lambda $recursion))))
      ((application? $application)
        (lets
          ($lhs (normalize (application-lhs $application)))
          ($rhs (normalize (application-rhs $application)))
          (switch $lhs
            ((lambda? $lambda)
              (lambda-apply $lambda $rhs))
            ((lambda-type? $lambda-type)
              (lambda-type-apply $lambda-type $rhs))
            ((recursion? $recursion)
              (normalize (application (recursion-apply $recursion $lhs) $rhs)))
            ((else $other)
              (application $lhs $rhs)))))
      ((branch? $branch)
        (lets
          ($condition (normalize (branch-condition $branch)))
          (switch $condition
            ((boolean? $boolean)
              (normalize (branch-ref $branch $boolean)))
            ((else $other)
              (branch $condition
                (normalize (branch-consequent $branch))
                (normalize (branch-alternate $branch)))))))))
)
