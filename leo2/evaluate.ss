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
          ($procedure (native-application-procedure $native-application))
          ($args (map evaluate (native-application-args $native-application)))
          (evaluated
            (if (for-all evaluated-native? $args)
              (native
                (apply $procedure
                  (map (dot native-ref evaluated-ref) $args)))
              (native-application $procedure $args)))))
      ((abstraction? $abstraction)
        (lets
          ($procedure (abstraction-procedure $abstraction))
          (evaluated
            (abstraction
              (lambda ($arg)
                (evaluate ($procedure $arg)))))))
      ((abstraction-type? $abstraction-type)
        (evaluated
          (abstraction-type
            (evaluate (abstraction-type-param $abstraction-type))
            (evaluated-ref (evaluate (abstraction-type-abstraction $abstraction-type))))))
      ((application? $application)
        (term-apply
          (evaluate (application-lhs $application))
          (evaluate (application-rhs $application))))
      ((recursive? $recursive)
        (lets
          ($procedure (abstraction-procedure (recursive-abstraction $recursive)))
          (evaluated
            (recursive
              (abstraction
                (lambda ($self)
                  (evaluate ($procedure $self))))))))))

  (define (term-apply $lhs $rhs)
    (switch (evaluated-ref $lhs)
      ((abstraction? $abstraction)
       ((abstraction-procedure $abstraction) $rhs))
      ((recursive? $recursive)
       (lets
        ($procedure (abstraction-procedure (recursive-abstraction $recursive)))
        ($value ($procedure $lhs))
        (term-apply $value $rhs)))
      ((else _)
       (evaluated (application $lhs $rhs)))))

  (define-rule-syntax (check-evaluates in out)
    (check
      (equal?
        (term->datum 0 (evaluate in))
        (term->datum 0 out))))
)
