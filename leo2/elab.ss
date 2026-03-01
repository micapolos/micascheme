(library (leo2 elab)
  (export
    task task-lets
    list->task task-append
    task->datum
    solutions-task->datum
    task-apply
    push-error-task
    push-hole-task
    solutions-task
    errors-task
    empty-env
    solutions
    errors

    elab-task
    eval-task
    solve-task

    check-task=?
    check-solutions-task=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal))

  (define empty-env '())

  (define (type-of $term)
    (switch $term
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((typed? $typed)
        (typed-type $typed))
      ((else _)
        unknown)))

  (define (value-of $term)
    (switch $term
      ((type? $type) $type)
      ((typed? $typed) (typed-ref $typed))
      ((else _) nothing)))

  (define (typed-from $type $value)
    ; (todo (collapse (and (type (+ n 1)) (type n)) (into (type n))))
    (typed $type $value))

  (define solutions stack)
  (define errors stack)

  (define-rules-syntaxes
    ((task ($solutions $errors) solutions-errors-result)
      (lambda ($solutions $errors) solutions-errors-result))
    ((task result)
      (task ($solutions $errors)
        (values $solutions $errors result)))
    ((task solutions errors result)
      (lambda ($unused-solutions $unused-errors)
        (values solutions errors result)))
    ((task-lets (id first) x ... last)
      (lambda ($solutions $errors)
        (lets
          ((values $solutions $errors id) (first $solutions $errors))
          ((task-lets x ... last) $solutions $errors))))
    ((task-lets x) x))

  (define-case-syntax (apply-task fn arg ...)
    (lets
      ($args #'(arg ...))
      ($tmps (generate-temporaries $args))
      #`(task-lets
        #,@(map-with
          ($tmp $tmps)
          ($arg $args)
          #`(#,$tmp #,$arg))
        (task (fn #,@$tmps)))))

  (define (task-result $task)
    (lets
      ((values $hole $errors $result) (task-apply $task '() '()))
      $result))

  (define (task-apply $task $solutions $errors)
    ($task $solutions $errors))

  (define solutions-task
    (task ($solutions $errors)
      (values $solutions $errors $solutions)))

  (define errors-task
    (task ($solutions $errors)
      (values $solutions $errors $errors)))

  (define new-hole-task
    (task ($solutions $errors)
      (values
        (push $solutions unknown)
        $errors
        (hole (length $solutions)))))

  (define-list->/append (task $tasks)
    (switch-exhaustive $tasks
      ((null? $null)
        (task $null))
      ((pair? $pair)
        (task-lets
          ($car (car $pair))
          ($cdr (list->task (cdr $pair)))
          (task (cons $car $cdr))))))

  (define (push-error-task $error $result)
    (task ($solutions $errors)
      (values
        $solutions
        (push $errors $error)
        $result)))

  (define (push-hole-task $hole $result)
    (task ($solutions $errors)
      (values
        (push $solutions $hole)
        $errors
        $result)))

  (define (task->datum $task)
    (solutions-task->datum '() $task))

  (define (solutions-task->datum $solutions $task)
    (lets
      ((values $solutions $errors $result) ($task $solutions '()))
      `(task
        (solutions ,@(reverse (map term->datum $solutions)))
        (errors ,@(reverse (map term->datum $errors)))
        (result ,(term->datum $result)))))

  (define (check-task $env $type $term)
    (task-lets
      ($typed (elab-task $env $term))
      ($type (solve-task $env $type (type-of $typed)))
      (task (typed-from $type (value-of $typed)))))

  (define (elab-task $env $term)
    (switch $term
      ((typed? $typed)
        (task $typed))
      ((ann? $ann)
        (check-task $env
          (ann-type $ann)
          (ann-ref $ann)))
      ((type? $type)
        (task $type))
      ((native-type? $native-type)
        (task (typed (type 0) $native-type)))
      ((native? $native)
        (task (typed native-type $native)))
      ((native-application? $native-application)
        (task-lets
          ($typed-args
            (list->task
              (map (partial check-task $env native-type)
                (native-application-args $native-application))))
          (task
            (typed
              (if (for-all native-type? (map type-of $typed-args))
                native-type
                nothing)
              (native-application
                (native-application-lambda $native-application)
                $typed-args)))))
      ((application? $application)
        (task-lets
          ($typed-lhs (elab-task $env (application-lhs $application)))
          ($typed-rhs (elab-task $env (application-rhs $application)))
          (switch (type-of $typed-lhs)
            ((lambda-type? $lambda-type)
              (task-lets
                ($type (solve-task $env
                  (lambda-type-param $lambda-type)
                  (type-of $typed-rhs)))
                (switch $type
                  ((nothing? $nothing)
                    (task
                      (typed nothing
                        (application $typed-lhs $typed-rhs))))
                  ((else $type)
                    (task
                      (typed
                        (lambda-type-apply $lambda-type $typed-rhs)
                        (application $typed-lhs $typed-rhs)))))))
            ((else $other)
              (task
                (typed
                  (typed (type 0)
                    (application
                      (type-of $typed-lhs)
                      (type-of $typed-rhs)))
                  (application $typed-lhs $typed-rhs)))))))
      ((variable? $variable)
        (switch (list-ref? $env (variable-index $variable))
          ((false? _)
            (push-error-task
              (unbound $variable)
              (typed nothing $variable)))
          ((else $type)
            (task (typed $type $variable)))))
      ((lambda-type? $lambda-type)
        (task-lets
          ($typed-param (elab-task $env (lambda-type-param $lambda-type)))
          ($body (task (lambda-type-apply $lambda-type (variable 0))))
          ($typed-body (elab-task (push $env (type-of $typed-param)) $body))
          ($max-depth
            (task
              (max
                (type-depth (type-of $typed-param))
                (type-depth (type-of $typed-body)))))
          (task
            (typed
              (type $max-depth)
              (lambda-type $typed-param
                (lambda ($arg)
                  (task-result
                    (elab-task
                      (push $env $typed-param)
                      (lambda-type-apply $lambda-type $arg)))))))))
      ((lambda? $lambda)
        (task-lets
          ($param-type new-hole-task)
          ($typed-body
            (elab-task
              (push $env $param-type)
              (variable 0)))
          (task
            (typed
              (lambda-type $param-type
                (lambda ($arg)
                  (type-of
                    (task-result
                      (elab-task
                        (push $env $param-type)
                        ($lambda $arg))))))
              (lambda ($arg)
                (task-result
                  (elab-task
                    (push $env $param-type)
                    ($lambda $arg))))))))))

  (define (solve-task $env $expected $actual)
    (or
      (switch $expected
        ((hole? $hole)
          (task ($solutions $errors)
            (lets
              ($index (- (length $solutions) (hole-index $hole) 1))
              (switch (list-ref? $solutions $index)
                ((false? _)
                  (values
                    $solutions
                    (push $errors (unbound $hole))
                    nothing))
                ((unknown? _)
                  (values
                    (list-set $solutions $index $actual)
                    $errors
                    $actual))
                ((else $other)
                  (task-apply
                    (solve-task $env $other $actual)
                    $solutions
                    $errors))))))
        ((native-type? _)
          (switch? $actual
            ((native-type? $native-type)
              (task $native-type))))
        ((type? $expected-type)
          (switch? $actual
            ((type? $actual-type)
              (and
                (=
                  (type-depth $expected-type)
                  (type-depth $actual-type))
                (task $actual-type)))))
        ; TODO: Cover all term types, and don't use term=?
        ((else _)
          (and
            (term=? $expected $actual)
            (task $actual))))
      (push-error-task
        (mismatch (expected $expected) (actual $actual))
        nothing)))

  (define (unpeel? $term)
    (switch? $term
      ((evaluated? $evaluated)
        (switch? (value-of (evaluated-ref $evaluated))
          ((evaluated? $evaluated)
            (evaluated-ref $evaluated))))))

  (define (eval-task $env $term)
    (switch $term
      ((evaluated? $evaluated)
        (task $evaluated))
      ((type? $type)
        (task (evaluated $type)))
      ((native-type? $native-type)
        (task (evaluated $native-type)))
      ((native? $native)
        (task (evaluated $native)))
      ((native-application? $native-application)
        (task-lets
          ($lambda
            (task
              (native-application-lambda $native-application)))
          ($evaluated-args
            (list->task
              (map
                (partial eval-task $env)
                (native-application-args $native-application))))
          ($unpeeled-args (task (map unpeel? $evaluated-args)))
          (task
            (evaluated
              (if (for-all native? $unpeeled-args)
                (native (apply $lambda (map native-ref $unpeeled-args)))
                (native-application $lambda $evaluated-args))))))
      ((typed? $typed)
        (apply-task evaluated
          (apply-task typed
            (eval-task $env (typed-type $typed))
            (eval-task $env (typed-ref $typed)))))
      ((variable? $variable)
        (task (evaluated $variable)))
      ((lambda-type? $lambda-type)
        (apply-task evaluated
          (apply-task lambda-type
            (eval-task $env (lambda-type-param $lambda-type))
            (eval-task $env (lambda-type-lambda $lambda-type)))))
      ((lambda? $lambda)
        (task
          (evaluated
            (lambda ($0)
              (task-result
                (eval-task
                  (push $env (type-of $0))
                  $0))))))
      ((application? $application)
        (task-lets
          ($evaluated-lhs (eval-task $env (application-lhs $application)))
          ($evaluated-rhs (eval-task $env (application-rhs $application)))
          (task
            (switch (unpeel? $evaluated-lhs)
              ((lambda? $lambda)
                ($lambda $evaluated-rhs))
              ((else $other-lhs)
                (evaluated (application $other-lhs $evaluated-rhs)))))))
      ((else $other)
        (throw eval-task $env $other))))

  (define-rule-syntax (check-task=? in out)
    (check
      (equal?
        (task->datum in)
        (task->datum out))))

  (define-rule-syntax (check-solutions-task=? sol in out)
    (check
      (equal?
        (solutions-task->datum sol in)
        (task->datum out))))
)
