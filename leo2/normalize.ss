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
            ((selector? $sel)
              $rhs)
            ((rejector? $rej)
              (normalize (rejector-ref $rej)))
            ((matcher? $m)
              (switcher $rhs))
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
              (lets
                ($data (switcher-ref $switcher))
                (switch $data
                  ((application? $app-data)
                    (normalize
                      (application
                        (switcher (application-lhs $app-data))
                        ;; IMPORTANT: We do NOT normalize the result of the logic yet.
                        ;; We build a naked application of (Logic RHS-Data)
                        (application $rhs (application-rhs $app-data)))))
                  ((native? $n)
                    (switch $rhs
                      ((selector? $sel) $n)
                      ((rejector? $rej) (normalize (rejector-ref $rej)))
                      ;; Ensure Logic is on the LHS and Data is on the RHS
                      ((else _) (normalize (application $rhs $n)))))
                  ((selector? $sel)
                    (normalize (application $rhs $data)))
                  ((else _)
                    (application $lhs $rhs)))))
            ((lambda? $lhs-lambda)
              (switch $rhs
                ((switcher? $s) (normalize (application $rhs $lhs)))
                ((else _) (normalize (lambda-apply $lhs-lambda $rhs)))))
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
