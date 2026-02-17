(library (leo2 evaluate)
  (export evaluate)
  (import
    (leo2 base)
    (leo2 term))

  (define (evaluate $term)
    (switch-exhaustive $term
      ((evaluated? $evaluated)
        $evaluated)
      ((native? $native)
        (evaluated $native))
      ((native-application? $native-application)
        (lets
          ($procedure (native-application-procedure $native-application))
          ($args (map evaluate (native-application-args $native-application)))
          (evaluated
            (if (for-all (and? evaluated? native?) $args)
              (native
                (apply $procedure
                  (map (dot native-ref evaluated-ref) $args)))
              (native-application $procedure $args)))))
      ((variable? $variable)
        (evaluated $variable))
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
)
