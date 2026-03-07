(library (leo2 normalize)
  (export
    normalize)
  (import
    (leo2 base)
    (leo2 term))

(define (normalize $term)
    (switch-exhaustive $term
      ((native? $native) $native)
      ((variable? $variable) $variable)
      ((lambda? $lambda)
        (lambda ($arg)
          (normalize (lambda-apply $lambda $arg))))
      ((lambda-type? $lambda-type)
        (lambda-type
          (normalize (lambda-type-param $lambda-type))
          (normalize (lambda-type-lambda $lambda-type))))
      ((recursion? $recursion)
        $recursion)
      ((application? $application)
        (lets
          ($lhs (normalize (application-lhs $application)))
          ($rhs (normalize (application-rhs $application)))
          (switch $lhs
            ((native? $lhs-native)
              (switch $rhs
                ((native? $rhs-native)
                  (native
                    (app
                      (native-ref $lhs-native)
                      (native-ref $rhs-native))))
                ((else $rhs-other)
                  (application $lhs $rhs))))
            ((lambda? $lhs-lambda)
              (normalize (lambda-apply $lhs-lambda $rhs)))
            ((lambda-type? $lhs-lambda-type)
              (normalize (lambda-type-apply $lhs-lambda-type $rhs)))
            ((recursion? $lhs-recursion)
              (normalize (application (recursion-apply $lhs-recursion $lhs) $rhs)))
            ((else _)
              (application $lhs $rhs)))))

      ((branch? $branch)
        (lets
          ($condition (normalize (branch-condition $branch)))
          (switch $condition
            ((native? $native)
              (normalize (branch-ref $branch (native-ref $native))))
            ((else $other)
              (branch $condition
                (normalize (branch-consequent $branch))
                (normalize (branch-alternate $branch)))))))))
)
