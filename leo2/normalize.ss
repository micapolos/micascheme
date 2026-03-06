(library (leo2 normalize)
  (export
    normalize)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 apply))

  (define (normalize $term)
    (switch-exhaustive $term
      ((native? $native) $native)
      ((neutral? $neutral) $neutral)
      ((primitive? $primitive) $primitive)
      ((variable? $variable) $variable)
      ((lambda? $lambda)
        (lambda ($arg)
          (normalize (lambda-apply $lambda $arg))))
      ((application? $application)
        (term-apply
          (normalize (application-lhs $application))
          (normalize (application-rhs $application))))
      ((recursion? $recursion)
        (recursion
          (normalize
            (recursion-lambda $recursion))))
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
