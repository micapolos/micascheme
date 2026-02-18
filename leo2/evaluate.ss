(library (leo2 evaluate)
  (export
    evaluate
    check-evaluates)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define (evaluated-native? $term)
    (switch? $term
      ((evaluated? $evaluated)
        (native? (evaluated-ref $evaluated)))))

  (define (evaluate $evaluate-native $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        $evaluated)
      ((variable? $variable)
        (evaluated $variable))
      ((type? $type)
        (evaluated $type))
      ((native? $native)
        (evaluated ($evaluate-native $native)))
      ((native-application? $native-application)
        (lets
          ($procedure
            (native-application-target $native-application))
          ($args
            (map
              (partial evaluate $evaluate-native)
              (native-application-args $native-application)))
          (evaluated
            (if (for-all evaluated-native? $args)
              (native
                (apply
                  ($evaluate-native $procedure)
                  (map (dot native-ref evaluated-ref) $args)))
              (native-application $procedure $args)))))
      ((abstraction? $abstraction)
        (evaluated
          (abstraction
            (lambda ($arg)
              (evaluate $evaluate-native
                (app
                  (abstraction-procedure $abstraction)
                  $arg))))))
      ((abstraction-type? $abstraction-type)
        (evaluated
          (abstraction-type
            (evaluate $evaluate-native (abstraction-type-param $abstraction-type))
            (lambda ($arg)
              (evaluate $evaluate-native
                (app
                  (abstraction-type-procedure $abstraction-type)
                  $arg))))))
      ((application? $application)
        (term-apply
          (evaluate $evaluate-native (application-lhs $application))
          (evaluate $evaluate-native (application-rhs $application))))
      ((recursive? $recursive)
        (evaluated
          (recursive
            (lambda ($self)
              (evaluate $evaluate-native
                (app
                  (recursive-procedure $recursive)
                  $self))))))
      ((branch? $branch)
        (lets
          ($condition (evaluate $evaluate-native (branch-condition $branch)))
          (if (evaluated-native? $condition)
            (evaluate $evaluate-native
              (if (native-ref (evaluated-ref $condition))
                (branch-consequent $branch)
                (branch-alternate $branch)))
            (evaluated
              (branch
                $condition
                (branch-consequent $branch)
                (branch-alternate $branch))))))))

  (define (term-apply $lhs $rhs)
    (switch (evaluated-ref $lhs)
      ((abstraction? $abstraction)
        (app (abstraction-procedure $abstraction) $rhs))
      ((recursive? $recursive)
        (term-apply (app (recursive-procedure $recursive) $lhs) $rhs))
      ((else _)
       (evaluated (application $lhs $rhs)))))

  (define-rule-syntax (check-evaluates in out)
    (check
      (equal?
        (term->datum 0 #f (evaluate identity in))
        (term->datum 0 #f out))))
)
