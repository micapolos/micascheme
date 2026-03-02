(library (leo2 elaborator)
  (export
    elaborator elaborator-lets
    list->elaborator elaborator-append
    elaborator->datum
    solutions-elaborator->datum
    elaborator-apply
    push-error-elaborator
    push-hole-elaborator
    solutions-elaborator
    errors-elaborator
    empty-env
    solutions
    errors

    term-elaborator
    eval-elaborator

    check-elaborator=?
    check-solutions-elaborator=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 solver)
    (leo2 evaluate)
    (leo2 equal))

  (define empty-env '())

  (define (type-of $term)
    (switch $term
      ((type? $type)
        (type+1 $type))
      ((typed? $typed)
        (typed-type $typed))
      ((evaluated? $evaluated)
        (evaluated
          (type-of
            (evaluated-ref $evaluated))))
      ((else _)
        unknown)))

  (define (value-of $term)
    (switch $term
      ((type? $type)
        $type)
      ((typed? $typed)
        (typed-ref $typed))
      ((evaluated? $evaluated)
        (evaluated
          (value-of
            (evaluated-ref $evaluated))))
      ((else _) nothing)))

  (define (typed-from $type $value)
    ; (todo (collapse (and (type (+ n 1)) (type n)) (into (type n))))
    (typed $type $value))

  (define solutions stack)
  (define errors stack)

  (define-rules-syntaxes
    ((elaborator ($solutions $errors) solutions-errors-result)
      (lambda ($solutions $errors) solutions-errors-result))
    ((elaborator result)
      (elaborator ($solutions $errors)
        (values $solutions $errors result)))
    ((elaborator solutions errors result)
      (lambda ($unused-solutions $unused-errors)
        (values solutions errors result)))
    ((elaborator-lets (id first) x ... last)
      (lambda ($solutions $errors)
        (lets
          ((values $solutions $errors id) (first $solutions $errors))
          ((elaborator-lets x ... last) $solutions $errors))))
    ((elaborator-lets x) x))

  (define-case-syntax (apply-elaborator fn arg ...)
    (lets
      ($args #'(arg ...))
      ($tmps (generate-temporaries $args))
      #`(elaborator-lets
        #,@(map-with
          ($tmp $tmps)
          ($arg $args)
          #`(#,$tmp #,$arg))
        (elaborator (fn #,@$tmps)))))

  (define (elaborator-result $elaborator)
    (lets
      ((values $hole $errors $result) (elaborator-apply $elaborator '() '()))
      $result))

  (define (elaborator-apply $elaborator $solutions $errors)
    ($elaborator $solutions $errors))

  (define solutions-elaborator
    (elaborator ($solutions $errors)
      (values $solutions $errors $solutions)))

  (define errors-elaborator
    (elaborator ($solutions $errors)
      (values $solutions $errors $errors)))

  (define new-hole-elaborator
    (elaborator ($solutions $errors)
      (values
        (push $solutions unknown)
        $errors
        (hole (length $solutions)))))

  (define-list->/append (elaborator $elaborators)
    (switch-exhaustive $elaborators
      ((null? $null)
        (elaborator $null))
      ((pair? $pair)
        (elaborator-lets
          ($car (car $pair))
          ($cdr (list->elaborator (cdr $pair)))
          (elaborator (cons $car $cdr))))))

  (define (push-error-elaborator $error $result)
    (elaborator ($solutions $errors)
      (values
        $solutions
        (push $errors $error)
        $result)))

  (define (push-hole-elaborator $hole $result)
    (elaborator ($solutions $errors)
      (values
        (push $solutions $hole)
        $errors
        $result)))

  (define (elaborator-with-solutions $solutions $result)
    (elaborator ($unused-solutions $errors)
      (values $solutions $errors $result)))

  (define (elaborator-with-errors $errors $result)
    (elaborator ($solutions $unused-errors)
      (values $solutions $errors $result)))

  (define (solutions-index $solutions $hole)
    (- (length $solutions) (hole-index $hole) 1))

  (define (solutions-ref-elaborator $hole)
    (elaborator-lets
      ($solutions solutions-elaborator)
      (switch (list-ref? $solutions (solutions-index $solutions $hole))
        ((false? _)
          (push-error-elaborator (unbound $hole) nothing))
        ((else $term)
          (elaborator $term)))))

  (define (solutions-set-elaborator $hole $term)
    (elaborator-lets
      ($solutions solutions-elaborator)
      (elaborator-with-solutions
        (list-set $solutions (solutions-index $solutions $hole) $term)
        $term)))

  (define (elaborator->datum $elaborator)
    (solutions-elaborator->datum '() $elaborator))

  (define (solutions-elaborator->datum $solutions $elaborator)
    (lets
      ((values $solutions $errors $result) ($elaborator $solutions '()))
      `(elaborator
        (solutions ,@(reverse (map term->datum $solutions)))
        (errors ,@(reverse (map term->datum $errors)))
        (result ,(term->datum $result)))))

  (define (check-elaborator $env $type $term)
    (elaborator-lets
      ($typed (term-elaborator $env $term))
      ($expected-type (transitional-eval-elaborator $env $type))
      ($actual-type (transitional-eval-elaborator $env (type-of $typed)))
      ($type (solve-elaborator $env $expected-type $actual-type))
      ($evaluated-type (transitional-eval-elaborator $env $type))
      (elaborator (typed-from $evaluated-type (value-of $typed)))))

  (define (term-elaborator $env $term)
    (switch $term
      ((typed? $typed)
        (elaborator $typed))
      ((ann? $ann)
        (check-elaborator $env
          (ann-type $ann)
          (ann-ref $ann)))
      ((type? $type)
        (elaborator $type))
      ((native-type? $native-type)
        (elaborator (typed (type 0) $native-type)))
      ((native? $native)
        (elaborator (typed native-type $native)))
      ((native-application? $native-application)
        (elaborator-lets
          ($typed-args
            (list->elaborator
              (map (partial check-elaborator $env native-type)
                (native-application-args $native-application))))
          (elaborator
            (typed
              (if (for-all native-type? (map (dot evaluated-ref type-of) $typed-args))
                (evaluated native-type)
                nothing)
              (native-application
                (native-application-lambda $native-application)
                $typed-args)))))
      ((application? $application)
        (elaborator-lets
          ($typed-lhs (term-elaborator $env (application-lhs $application)))
          ($typed-rhs (term-elaborator $env (application-rhs $application)))
          (switch (type-of $typed-lhs)
            ((lambda-type? $lambda-type)
              (elaborator-lets
                ($eval-param (transitional-eval-elaborator $env (lambda-type-param $lambda-type)))
                ($eval-rhs-type (transitional-eval-elaborator $env (type-of $typed-rhs)))
                ($type (solve-elaborator $env $eval-param $eval-rhs-type))
                (switch $type
                  ((nothing? $nothing)
                    (elaborator
                      (typed
                        (evaluated nothing)
                        (application $typed-lhs $typed-rhs))))
                  ((else $type)
                    (elaborator
                      (typed
                        (lambda-type-apply $lambda-type $typed-rhs)
                        (application $typed-lhs $typed-rhs)))))))
            ((else $other)
              (elaborator
                (typed
                  (typed (type 0)
                    (application
                      (type-of $typed-lhs)
                      (type-of $typed-rhs)))
                  (application $typed-lhs $typed-rhs)))))))
      ((variable? $variable)
        (switch (list-ref? $env (variable-index $variable))
          ((false? _)
            (push-error-elaborator
              (unbound $variable)
              (typed nothing $variable)))
          ((else $type)
            (elaborator (typed $type $variable)))))
      ((lambda-type? $lambda-type)
        (elaborator-lets
          ($typed-param (term-elaborator $env (lambda-type-param $lambda-type)))
          ($body (elaborator (lambda-type-apply $lambda-type (variable 0))))
          ($typed-body (term-elaborator (push $env (type-of $typed-param)) $body))
          ($max-depth
            (elaborator
              (max
                (type-depth (type-of $typed-param))
                (type-depth (type-of $typed-body)))))
          (elaborator
            (typed
              (type $max-depth)
              (lambda-type $typed-param
                (lambda ($arg)
                  (elaborator-result
                    (term-elaborator
                      (push $env $typed-param)
                      (lambda-type-apply $lambda-type $arg)))))))))
      ((lambda? $lambda)
        (elaborator-lets
          ($param-type new-hole-elaborator)
          ($typed-body
            (term-elaborator
              (push $env $param-type)
              (variable 0)))
          (elaborator
            (typed
              (lambda-type $param-type
                (lambda ($arg)
                  (type-of
                    (elaborator-result
                      (term-elaborator
                        (push $env $param-type)
                        ($lambda $arg))))))
              (lambda ($arg)
                (elaborator-result
                  (term-elaborator
                    (push $env $param-type)
                    ($lambda $arg))))))))))

  (define (eval-solve-elaborator $env $expected $actual)
    (elaborator-lets
      ($eval-expected (transitional-eval-elaborator $env $expected))
      ($eval-actual (transitional-eval-elaborator $env $actual))
      (solve-elaborator $env $eval-expected $eval-actual)))

  (define (solve-elaborator $env $expected $actual)
    (elaborator ($solutions $errors)
      (lets
        ((values $solutions $result)
          (solver-apply
            (term-solver (length $env) $expected $actual)
            $solutions))
        (values $solutions $errors $result))))

  (define (unpeel? $term)
    (switch? $term
      ((evaluated? $evaluated)
        (switch? (value-of (evaluated-ref $evaluated))
          ((evaluated? $evaluated)
            (evaluated-ref $evaluated))))))

  (define (transitional-eval-elaborator $env $term)
    (lets
      ($old (eval-elaborator $env $term))
      ($new (new-eval-elaborator $env $term))
      ; (run
      ;   (pretty-print `(old ,(elaborator-result $old)))
      ;   (pretty-print `(new ,(elaborator-result $new))))
      $old))

  (define (new-eval-elaborator $env $term)
    (elaborator (evaluate $term)))

  (define (eval-elaborator $env $term)
    (switch $term
      ((evaluated? $evaluated)
        (elaborator $evaluated))
      ((nothing? $nothing)
        (elaborator (evaluated $nothing)))
      ((type? $type)
        (elaborator (evaluated $type)))
      ((native-type? $native-type)
        (elaborator (evaluated $native-type)))
      ((native? $native)
        (elaborator (evaluated $native)))
      ((native-application? $native-application)
        (elaborator-lets
          ($lambda
            (elaborator
              (native-application-lambda $native-application)))
          ($evaluated-args
            (list->elaborator
              (map
                (partial eval-elaborator $env)
                (native-application-args $native-application))))
          ($unpeeled-args (elaborator (map unpeel? $evaluated-args)))
          (elaborator
            (evaluated
              (if (for-all native? $unpeeled-args)
                (native (apply $lambda (map native-ref $unpeeled-args)))
                (native-application $lambda $evaluated-args))))))
      ((typed? $typed)
        (apply-elaborator evaluated
          (apply-elaborator typed
            (eval-elaborator $env (typed-type $typed))
            (eval-elaborator $env (typed-ref $typed)))))
      ((variable? $variable)
        (elaborator (evaluated $variable)))
      ((lambda-type? $lambda-type)
        (apply-elaborator evaluated
          (apply-elaborator lambda-type
            (eval-elaborator $env (lambda-type-param $lambda-type))
            (eval-elaborator $env (lambda-type-lambda $lambda-type)))))
      ((lambda? $lambda)
        (elaborator
          (evaluated
            (lambda ($0)
              (elaborator-result
                (eval-elaborator
                  (push $env (type-of $0))
                  $0))))))
      ((application? $application)
        (elaborator-lets
          ($evaluated-lhs (eval-elaborator $env (application-lhs $application)))
          ($evaluated-rhs (eval-elaborator $env (application-rhs $application)))
          (elaborator
            (switch (unpeel? $evaluated-lhs)
              ((lambda? $lambda)
                ($lambda $evaluated-rhs))
              ((else $other-lhs)
                (evaluated (application $other-lhs $evaluated-rhs)))))))
      ((else $other)
        (throw eval-elaborator $env $other))))

  (define-rule-syntax (check-elaborator=? in out)
    (check
      (equal?
        (elaborator->datum in)
        (elaborator->datum out))))

  (define-rule-syntax (check-solutions-elaborator=? sol in out)
    (check
      (equal?
        (solutions-elaborator->datum sol in)
        (elaborator->datum out))))
)
