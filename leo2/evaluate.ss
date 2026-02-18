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

  (define (evaluate $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        $evaluated)
      ((variable? $variable)
        (evaluated $variable))
      ((type? $type)
        (evaluated $type))
      ((native? $native)
        (evaluated $native))
      ((native-application? $native-application)
        (lets
          ($procedure (evaluate (native-application-target $native-application)))
          ($args (map evaluate (native-application-args $native-application)))
          (evaluated
            (if (for-all evaluated-native? (cons $procedure $args))
              (native
                (apply
                  (native-ref (evaluated-ref $procedure))
                  (map (dot native-ref evaluated-ref) $args)))
              (native-application $procedure $args)))))
      ((abstraction? $abstraction)
        (evaluated
          (abstraction
            (lambda ($arg)
              (evaluate
                (app
                  (abstraction-procedure $abstraction)
                  $arg))))))
      ((abstraction-type? $abstraction-type)
        (evaluated
          (abstraction-type
            (evaluate (abstraction-type-param $abstraction-type))
            (lambda ($arg)
              (evaluate
                (app
                  (abstraction-type-procedure $abstraction-type)
                  $arg))))))
      ((application? $application)
        (term-apply
          (evaluate (application-lhs $application))
          (evaluate (application-rhs $application))))
      ((recursive? $recursive)
        (evaluated
          (recursive
            (lambda ($self)
              (evaluate
                (app
                  (recursive-procedure $recursive)
                  $self))))))
      ((branch? $branch)
        (lets
          ($condition (evaluate (branch-condition $branch)))
          (if (evaluated-native? $condition)
            (evaluate
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
        (term->datum 0 #f (evaluate in))
        (term->datum 0 #f out))))
)
