(library (leo2 deduce)
  (export
    deduction
    failed-deduction
    deduced-deduction
    push-deduction
    deduction-with

    deduction-bind
    deduction-lets

    deduce

    term-deduction->datum
    term-deduction-from-to

    check-term-deduction
    check-term-deduction-from-to)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define-rule-syntax (deduction ($deduced) body)
    (lambda ($deduced) body))

  (define failed-deduction
    (deduction ($deduced)
      (values #f $deduced)))

  (define deduced-deduction
    (lambda ($deduced)
      (values $deduced $deduced)))

  (define (push-deduction $hole $term)
    (lambda ($deduced)
      (values $term
        (push $deduced (cons $hole $term)))))

  (define (deduction-with $value)
    (switch $value
      ((false? $false)
        (throw deduction-with $false))
      ((else $value)
        (deduction ($deduced)
          (values $value $deduced)))))

  (define (deduction-bind $deduction $fn)
    (lambda ($deduced)
      (lets
        ((values $val? $deduced) ($deduction $deduced))
        (switch $val?
          ((false? $false) (values $false $deduced))
          ((else $val) (($fn $val) $deduced))))))

  (define-rules-syntax
    ((deduction-lets deduction) deduction)
    ((deduction-lets (id deduction) x ...)
      (deduction-bind deduction
        (lambda (id) (deduction-lets x ...)))))

  (define (deduce $deduction)
    ($deduction '()))

  (define (deduced-resolve $deduced $term)
    (switch $term
      ((hole? $term)
        (switch (assoc $term $deduced)
          ((false? _) $term)
          ((else $ass)
            (deduced-resolve $deduced (cdr $ass)))))
      ((else $term) $term)))

  (define-recursive (term-deduction-from-to $env $source $target)
    (deduction-lets
      ($deduction deduced-deduction)
      (lets
        ($source (deduced-resolve $deduction $source))
        ($target (deduced-resolve $deduction $target))
        (switch $target
          ((hole? $target-hole)
            (push-deduction $target-hole
              (deduced-resolve $deduction $source)))
          ((else $target)
            (or
              (term-switch $source
                ((hole? $source-hole)
                  (push-deduction $source-hole $target))

                ((nothing? _)
                  (switch? $target
                    ((nothing? _) (deduction-with $source))))

                ((type? $source-type)
                  (switch? $target
                    ((type? $target-type)
                      (and
                        (=
                          (type-depth $source-type)
                          (type-depth $target-type))
                        (deduction-with $source-type)))))

                ((native? $source-native)
                  (switch? $target
                    ((native? $target-native)
                      (and
                        (equal?
                          (native-ref $source-native)
                          (native-ref $target-native))
                        (deduction-with $source-native)))))

                ((native-application? $source-native-application)
                  (switch? $target
                    ((native-application? $target-native-application)
                      (and
                        (eq?
                          (native-application-lambda $source-native-application)
                          (native-application-lambda $target-native-application))
                        (lets-recursive deduce-args
                          ($args-s (native-application-args $source-native-application))
                          ($args-t (native-application-args $target-native-application))
                          (cond
                            ((and (null? $args-s) (null? $args-t))
                              (deduction-with $source))
                            ((and (pair? $args-s) (pair? $args-t))
                              (deduction-lets
                                (_ (term-deduction-from-to $env (car $args-s) (car $args-t)))
                                (deduce-args (cdr $args-s) (cdr $args-t))))
                            (else failed-deduction)))))))

                ((variable? $source-variable)
                  (lets
                    ($source (list-ref $env (variable-index $source-variable)))
                    (term-deduction-from-to $env $source $target)))

                ((lambda? $source-lambda)
                  (switch? $target
                    ((lambda? $target-lambda)
                      (term-deduction-from-to
                        (push $env nothing)
                        ($source-lambda (variable 0))
                        ($target-lambda (variable 0))))))

                ((lambda-type? $source-lambda-type)
                  (switch? $target
                    ((lambda-type? $target-lambda-type)
                      (deduction-lets
                        (_
                          (term-deduction-from-to $env
                            (lambda-type-param $target-lambda-type)
                            (lambda-type-param $source-lambda-type)))
                        (term-deduction-from-to
                          (push $env (lambda-type-param $target-lambda-type))
                          (lambda-type-apply $source-lambda-type (variable 0))
                          (lambda-type-apply $target-lambda-type (variable 0)))))))

                ((application? $source-application)
                  (switch? $target
                    ((application? $target-application)
                      (deduction-lets
                        ($lhs
                          (term-deduction-from-to $env
                            (application-lhs $source-application)
                            (application-lhs $target-application)))
                        ($rhs
                          (term-deduction-from-to $env
                            (application-rhs $source-application)
                            (application-rhs $target-application)))
                        (deduction-with (application $lhs $rhs))))))

                ((branch? $source-branch)
                  (switch? $target
                    ((branch? $target-branch)
                      (deduction-lets
                        (_
                          (term-deduction-from-to $env
                            (branch-condition $source-branch)
                            (branch-condition $target-branch)))
                        (_
                          (term-deduction-from-to $env
                            (branch-consequent $source-branch)
                            (branch-consequent $target-branch)))
                        (term-deduction-from-to $env
                          (branch-alternate $source-branch)
                          (branch-alternate $target-branch))))))

                ((recursion? $source-recursion)
                  (switch? $target
                    ((recursion? $target-recursion)
                      (term-deduction-from-to $env
                        (recursion-lambda $source-recursion)
                        (recursion-lambda $target-recursion)))))

                ((labeled? $source-labeled)
                  (switch? $target
                    ((labeled? $target-labeled)
                      (and
                        (eq?
                          (labeled-label $source-labeled)
                          (labeled-label $target-labeled))
                        (term-deduction-from-to $env
                          (labeled-ref $source-labeled)
                          (labeled-ref $target-labeled))))))

                ((evaluated? $source-evaluated)
                  (switch? $target
                    ((evaluated? $target-evaluated)
                      (term-deduction-from-to $env
                        (evaluated-ref $source-evaluated)
                        (evaluated-ref $target-evaluated)))))

                ((typed? $source-typed)
                  (switch? $target
                    ((typed? $target-typed)
                      (deduction-lets
                        (_
                          (term-deduction-from-to $env
                            (typed-type $source-typed)
                            (typed-type $target-typed)))
                        (term-deduction-from-to $env
                          (typed-ref $source-typed)
                          (typed-ref $target-typed)))))))
              failed-deduction))))))

  (define (term-deduction->datum $term-deduction)
    (lets
      ((values $term? $deduced)
        (deduce $term-deduction))
      `(deduction
        ,@(map-with ($entry $deduced)
          `(hole
            ,(hole-index (car $entry))
            ,(term->datum (cdr $entry))))
        ,(switch $term?
          ((false? $false) $false)
          ((else $term) (term->datum $term))))))

  (define-rule-syntax (check-term-deduction deduction out)
    (check (equal? (term-deduction->datum deduction) 'out)))

  (define-rule-syntax (check-term-deduction-from-to from to out)
    (check-term-deduction (term-deduction-from-to '() from to) out))
)
