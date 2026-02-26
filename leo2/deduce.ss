(library (leo2 deduce)
  (export
    deduction
    deduction-bind
    deduction-lets

    deduction-to
    check-deduces-to)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define-rule-syntax (deduction ($deduced) expr)
    (lambda ($deduced)
      (values expr $deduced)))

  (define (deduction-with $val)
    (switch $val
      ((false? $false)
        (throw deduction-with $false))
      ((else $val)
        (deduction ($deduced)
          (values $val $deduced)))))

  (define failed-deduction
    (deduction ($deduced)
      (values #f $deduced)))

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

  (define deduced-deduction
    (lambda ($deduced)
      (values $deduced $deduced)))

  (define (push-deduction $hole $term)
    (lambda ($deduced)
      (values $term
        (push $deduced (cons $hole $term)))))

  (define (deduced-resolve $deduced $term)
    (switch $term
      ((hole? $term)
        (switch (assoc $term $deduced)
          ((false? _) $term)
          ((else $ass)
            (deduced-resolve $deduced (cdr $ass)))))
      ((else $term) $term)))

  (define-recursive (deduction-to $env $source $target)
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
                        (deduction-with $source)))))

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
                          (native-application-procedure $source-native-application)
                          (native-application-procedure $target-native-application))
                        (lets-recursive deduce-args
                          ($args-s (native-application-args $source-native-application))
                          ($args-t (native-application-args $target-native-application))
                          (cond
                            ((and (null? $args-s) (null? $args-t))
                              (deduction-with $source))
                            ((and (pair? $args-s) (pair? $args-t))
                              (deduction-lets
                                (_ (deduction-to $env (car $args-s) (car $args-t)))
                                (deduce-args (cdr $args-s) (cdr $args-t))))
                            (else #f)))))))

                ((variable? $source-variable)
                  (lets
                    ($source (list $env (variable-index $source-variable)))
                    (deduction-to $env $source $target)))

                ((procedure? $source-procedure)
                  (switch? $target
                    ((procedure? $target-procedure)
                      (deduction-to
                        (push $env nothing)
                        ($source-procedure (variable 0))
                        ($target-procedure (variable 0))))))

                ((signature? $source-signature)
                  (switch? $target
                    ((signature? $target-signature)
                      (deduction-lets
                        (_
                          (deduction-to $env
                            (signature-param $target-signature)
                            (signature-param $source-signature)))
                        (deduction-to
                          (push $env (signature-param $target-signature))
                          (signature-apply $source-signature (variable 0))
                          (signature-apply $target-signature (variable 0)))))))

                ((application? $source-application)
                  (switch? $target
                    ((application? $target-application)
                      (deduction-lets
                        (_
                          (deduction-to $env
                            (application-lhs $source-application)
                            (application-lhs $target-application)))
                        (deduction-to $env
                          (application-rhs $source-application)
                          (application-rhs $target-application))))))

                ((branch? $source-branch)
                  (switch? $target
                    ((branch? $target-branch)
                      (deduction-lets
                        (_
                          (deduction-to $env
                            (branch-condition $source-branch)
                            (branch-condition $target-branch)))
                        (_
                          (deduction-to $env
                            (branch-consequent $source-branch)
                            (branch-consequent $target-branch)))
                        (deduction-to $env
                          (branch-alternate $source-branch)
                          (branch-alternate $target-branch))))))

                ((recursion? $source-recursion)
                  (switch? $target
                    ((recursion? $target-recursion)
                      (deduction-to $env
                        (recursion-procedure $source-recursion)
                        (recursion-procedure $target-recursion)))))

                ((labeled? $source-labeled)
                  (switch? $target
                    ((labeled? $target-labeled)
                      (and
                        (eq?
                          (labeled-label $source-labeled)
                          (labeled-label $target-labeled))
                        (deduction-to $env
                          (labeled $source-labeled)
                          (labeled $target-labeled))))))

                ((evaluated? $source-evaluated)
                  (switch? $target
                    ((evaluated? $target-evaluated)
                      (deduction-to $env
                        (evaluated-ref $source-evaluated)
                        (evaluated-ref $target-evaluated)))))

                ((typed? $source-typed)
                  (switch? $target
                    ((typed? $target-typed)
                      (deduction-lets
                        (_
                          (deduction-to $env
                            (typed-type $source-typed)
                            (typed-type $target-typed)))
                        (deduction-to $env
                          (typed-ref $source-typed)
                          (typed-ref $target-typed)))))))
              failed-deduction))))))

  (define-rule-syntax (check-deduces-to source target out)
    (check-term->datum=?
      (app (deduction-to '() source target) '())
      'out))
)
