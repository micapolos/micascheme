(library (leo2 evaluate)
  (export
    evaluate
    check-evaluates)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal))

  (define (evaluated-typed-native? $term)
    (switch? $term
      ((evaluated? $evaluated)
        (switch? (evaluated-ref $evaluated)
          ((typed? $typed)
            (native? (typed-ref $typed)))))))

  (define (evaluate $term)
    (term-switch $term
      ((evaluated? $evaluated)
        $evaluated)
      ((type? $type)
        (evaluated $term))
      ((typed? $typed)
        (type-evaluate
          (typed-type $typed)
          (typed-ref $typed)))))

  (define (type-evaluate $type $term)
    (term-ref-switch $term
      ((boolean? $boolean)
        (evaluated (typed $type $boolean)))
      ((number? $number)
        (evaluated (typed $type $number)))
      ((char? $char)
        (evaluated (typed $type $char)))
      ((string? $string)
        (evaluated (typed $type $string)))
      ((symbol? $symbol)
        (evaluated (typed $type $symbol)))
      ((indexed? $indexed)
        (evaluated
          (typed $type
            (indexed
              (indexed-index $indexed)
              (evaluate (indexed-ref $indexed))))))
      ((symbolic? $symbolic)
        (evaluated
          (typed $type
            (symbolic
              (symbolic-symbol $symbolic)
              (evaluate (symbolic-ref $symbolic))))))
      ((native? $native)
        (evaluated (typed $type $term)))
      ((native-application? $native-application)
        (lets
          ($procedure (native-application-procedure $native-application))
          ($args (map evaluate (native-application-args $native-application)))
          (evaluated
            (typed $type
              (if (for-all evaluated-typed-native? $args)
                (native
                  (apply $procedure
                    (map (dot native-ref typed-ref evaluated-ref) $args)))
                (native-application $procedure $args))))))
      ((variable? $variable)
        (evaluated (typed $type $variable)))
      ((abstraction? $abstraction)
        (evaluated
          (typed $type
            (abstraction
              (lambda ($arg)
                (evaluate
                  (app
                    (abstraction-procedure $abstraction)
                    $arg)))))))
      ((abstraction-type? $abstraction-type)
        (evaluated
          (typed $type
            (abstraction-type
              (evaluate (abstraction-type-param $abstraction-type))
              (lambda ($arg)
                (evaluate
                  (app
                    (abstraction-type-procedure $abstraction-type)
                    $arg)))))))
      ((application? $application)
        (term-apply
          $type
          (evaluate (application-lhs $application))
          (evaluate (application-rhs $application))))
      ((branch? $branch)
        (lets
          ($condition (evaluate (branch-condition $branch)))
          ($consequent (evaluate (branch-consequent $branch)))
          ($alternate (evaluate (branch-alternate $branch)))
          (if (evaluated-typed-native? $condition)
            (if (native-ref (typed-ref (evaluated-ref $condition)))
              $consequent
              $alternate)
            (if (term=? 0 $consequent $alternate)
              $consequent
              (evaluated
                (typed $type
                  (branch $condition $consequent $alternate)))))))
      ((recursion? $recursion)
        (evaluated
          (typed $type
            (recursion
              (lambda ($self)
                (evaluate
                  (app
                    (recursion-procedure $recursion)
                    $self)))))))))

  (define (term-apply $type $lhs $rhs)
    (switch (typed-ref (evaluated-ref $lhs))
      ((abstraction? $abstraction)
        (app (abstraction-procedure $abstraction) $rhs))
      ((recursion? $recursion)
        (term-apply $type (app (recursion-procedure $recursion) $lhs) $rhs))
      ((else _)
       (evaluated (typed $type (application $lhs $rhs))))))

  (define-rule-syntax (check-evaluates in out)
    (check
      (equal?
        (term->datum #f #f 0 (evaluate in))
        (term->datum #f #f 0 out))))
)
