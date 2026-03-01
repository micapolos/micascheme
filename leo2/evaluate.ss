(library (leo2 evaluate)
  (export
    evaluate
    check-evaluates)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal))

  (define (evaluated-native? $term)
    (switch? $term
      ((evaluated? $evaluated)
        (native?
          (evaluated-ref $evaluated)))))

  (define (evaluate $term)
    (term-switch $term
      ((hole? $hole)
        (evaluated $hole))
      ((nothing? $nothing)
        (evaluated $nothing))
      ((type? $type)
        (evaluated $term))
      ((native? $native)
        (evaluated $native))
      ((native-application? $native-application)
        (lets
          ($lambda (native-application-lambda $native-application))
          ($evaluated-args (map evaluate (native-application-args $native-application)))
          ($args (map evaluated-ref $evaluated-args))
          (evaluated
            (if (for-all native? $args)
              (native (apply $lambda (map native-ref $args)))
              (native-application $lambda $evaluated-args)))))
      ((variable? $variable)
        (evaluated $variable))
      ((lambda? $lambda)
        (evaluated
          (lambda ($arg)
            (evaluate ($lambda $arg)))))
      ((lambda-type? $lambda-type)
        (evaluated
          (lambda-type
            (evaluate (lambda-type-param $lambda-type))
            (lambda ($arg)
              (evaluate
                (lambda-type-apply $lambda-type $arg))))))
      ((application? $application)
        (term-apply
          (evaluate (application-lhs $application))
          (evaluate (application-rhs $application))))
      ((branch? $branch)
        (lets
          ($condition (evaluate (branch-condition $branch)))
          (if (evaluated-native? $condition)
            (evaluate
              (branch-ref $branch
                (native-ref (evaluated-ref $condition))))
            (lets
              ($consequent (evaluate (branch-consequent $branch)))
              ($alternate (evaluate (branch-alternate $branch)))
              (if (term=? $consequent $alternate)
                $consequent
                (evaluated (branch $condition $consequent $alternate)))))))
      ((recursion? $recursion)
        (evaluated
          (recursion
            (lambda ($arg)
              (evaluate
                (recursion-apply $recursion $arg))))))
      ((labeled? $labeled)
        (evaluated
          (labeled
            (evaluate (labeled-label $labeled))
            (evaluate (labeled-ref $labeled)))))
      ((evaluated? $evaluated)
        $evaluated)
      ((typed? $typed)
        (evaluated
          (typed
            (evaluate (typed-type $typed))
            (evaluate (typed-ref $typed)))))))

  (define (term-apply $lhs $rhs)
    (switch (evaluated-ref $lhs)
      ((lambda? $lambda)
        ($lambda $rhs))
      ((lambda-type? $lambda-type)
        (lambda-type-apply $lambda-type $rhs))
      ((recursion? $recursion)
        (term-apply (recursion-apply $recursion $lhs) $rhs))
      ((else _)
       (evaluated (application $lhs $rhs)))))

  (define-rule-syntax (check-evaluates in out)
    (check
      (equal?
        (term->datum (evaluate in))
        (term->datum out))))
)
