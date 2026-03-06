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
      ((selector? $selector) $selector)
      ((rejector? $rejector) $rejector)
      ((matcher? $matcher) $matcher)
      ((switcher? $s) (switcher (normalize (switcher-ref $s))))
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
            ((application? $lhs-application)
              (switch (application-lhs $lhs-application)
                ((matcher? $matcher)
                  (normalize
                    (application
                      (switcher (application-rhs $lhs-application))
                      $rhs)))
                ((else $other)
                  (application $lhs $rhs))))
            ((switcher? $switcher)
              (lets ($data (switcher-ref $switcher))
                (switch $data
                  ((application? $app-data)
                   (switch $rhs
                     ((application? $app-logic)
                      (normalize
                        (application
                          (switcher (application-lhs $app-data))
                          (lambda-apply (application-rhs $app-logic) (application-rhs $app-data)))))
                     ((lambda? $lam-logic)
                      (normalize
                        (application
                          (switcher (application-lhs $app-data))
                          (lambda-apply $lam-logic (application-rhs $app-data)))))
                     ((else _) (application $lhs $rhs))))
                  ((selector? $sel)
                   (switch $rhs
                     ((application? $app-logic)
                      (normalize (lambda-apply (application-rhs $app-logic) $data)))
                     ((lambda? $lam-logic)
                      (normalize (lambda-apply $lam-logic $data)))
                     ((else $val) $val)))
                  ((rejector? $rej)
                   (switch $rhs
                     ((application? $app-logic)
                      ;; SUCCESS: Skip the RHS branch, move to the LHS branch
                      (normalize (application (switcher (rejector-ref $rej)) (application-lhs $app-logic))))
                     ((else _)
                      ;; If there's no more application spine to peel,
                      ;; just pass the switcher reference forward.
                      (normalize (application (switcher (rejector-ref $rej)) $rhs)))))
                  ((else _) (application $lhs $rhs)))))

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
