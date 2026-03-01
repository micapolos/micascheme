(library (leo2 elab)
  (export
    task task-lets
    list->task task-append
    task->datum
    error-task
    check-task=?
    empty-env

    elab-task

    elab
    check-elabs
    check-evaluates

    check-elab-task=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal))

  (define empty-env '())

  (define (type-of $term)
    (switch-exhaustive $term
      ((type? $type)
        (type (+ (type-depth $type) 1)))
      ((typed? $typed)
        (typed-type $typed))))

  (define (value-of $term)
    (switch-exhaustive $term
      ((type? $type) $type)
      ((typed? $typed) (typed-ref $typed))))

  (define (typed-from $type $value)
    ; (todo (collapse (and (type (+ n 1)) (type n)) (into (type n))))
    (typed $type $value))

  (define-rules-syntaxes
    ((task ($solutions $errors) solutions-errors-result)
      (lambda ($solutions $errors) solutions-errors-result))
    ((task result)
      (task ($solutions $errors)
        (values $solutions $errors result)))
    ((task-lets (id first) x ... last)
      (lambda ($solutions $errors)
        (lets
          ((values $solutions $errors id) (first $solutions $errors))
          ((task-lets x ... last) $solutions $errors))))
    ((task-lets x) x))

  (define-list->/append (task $tasks)
    (switch-exhaustive $tasks
      ((null? $null)
        (task $null))
      ((pair? $pair)
        (task-lets
          ($car (car $pair))
          ($cdr (list->task (cdr $pair)))
          (task (cons $car $cdr))))))

  (define (error-task $error $result)
    (task ($solutions $errors)
      (values
        $solutions
        (push $errors $error)
        $result)))

  (define (task->datum $task)
    (lets
      ((values $solutions $errors $result) ($task '() '()))
      `(task
        (solutions ,@(reverse (map term->datum $solutions)))
        (errors ,@(reverse (map term->datum $errors)))
        (result ,(term->datum $result)))))

  (define (elab* $meta-context $context $terms)
    (switch-exhaustive $terms
      ((null? $null)
        (values $meta-context $null))
      ((pair? $pair)
        (lets
          ((values $meta-context $typed-car)
            (elab $meta-context $context (car $pair)))
          ((values $meta-context $typed-cdr)
            (elab* $meta-context $context (cdr $pair)))
          (values $meta-context
            (cons $typed-car $typed-cdr))))))

  (define (check-task $env $type $term)
    (task-lets
      ($typed (elab-task $env $term))
      ($type (resolve-task $env $type (type-of $typed)))
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
                (native-application-procedure $native-application)
                $typed-args)))))
      ((variable? $variable)
        (switch (list-ref? $env (variable-index $variable))
          ((false? _)
            (error-task "unbound variable"
              (typed nothing $variable)))
          ((else $type)
            (task (typed $type $variable)))))))

  (define (elab $meta-context $context $term)
    (switch $term
      ((typed? $typed)
        (values $meta-context $typed))

      ((ann? $ann)
        (lets
          ((values $meta-context $typed)
            (elab $meta-context $context (ann-ref $ann)))
          ((values $meta-context $type)
            (meta-resolve $meta-context $context
              (ann-type $ann)
              (typed-type $typed)))
          (values $meta-context
            (typed $type (typed-ref $typed)))))

      ((native? $native)
        (values $meta-context
          (typed native-type $native)))

      ((native-application? $native-application)
        (lets
          ((values $meta-context $typed-args)
            (elab* $meta-context $context
              (native-application-args $native-application)))
          (values $meta-context
            (typed native-type
              (native-application
                (native-application-procedure $native-application)
                $typed-args)))))

      ((type? $type)
        (values $meta-context
          (typed
            (type (+ (type-depth $type) 1))
            $type)))

      ((native-type? $native-type)
        (values $meta-context
          (typed (type 0) $native-type)))

      ((signature? $signature)
        (lets
          ((values $meta-context $typed-param)
            (elab $meta-context $context
              (signature-param $signature)))
          ($body (signature-apply $signature (variable 0)))
          ((values $meta-context $typed-body)
            (elab $meta-context
              (push $context (typed-type $typed-param))
              $body))
          ($max-depth
            (max
              (type-depth (typed-type $typed-param))
              (type-depth (typed-type $typed-body))))
          (values $meta-context
            (typed
              (type $max-depth)
              (signature $typed-param
                (lambda ($arg)
                  (lets
                    ((values _ $typed-body)
                      (elab $meta-context
                        (push $context (typed-type $typed-param))
                        (signature-apply $signature $arg)))
                    $typed-body)))))))

      ((variable? $variable)
        (values $meta-context
          (typed
            (list-ref $context (variable-index $variable))
            $variable)))

      ((procedure? $procedure)
        (lets
          ($param-type (hole (length $meta-context)))
          ($meta-context (push $meta-context unknown))
          ($body-context (push $context $param-type))
          ($body ($procedure (variable 0)))
          ((values $meta-context $typed-body)
            (elab $meta-context $body-context $body))
          (values $meta-context
            (typed
              (signature $param-type
                (lambda ($arg)
                  (lets
                    ((values _ $typed-body)
                      (elab $meta-context
                        (push $context $param-type)
                        ($procedure $arg)))
                    (typed-type $typed-body))))
              (lambda ($arg)
                (lets
                  ((values _ $typed-body)
                    (elab $meta-context
                      (push $context $param-type)
                      ($procedure $arg)))
                  $typed-body))))))

      ((application? $application)
        (lets
          ((values $meta-context $typed-lhs)
            (elab $meta-context $context (application-lhs $application)))
          ((values $meta-context $typed-rhs)
            (elab $meta-context $context (application-rhs $application)))
          (switch (typed-type $typed-lhs)
            ((signature? $signature)
              (lets
                ((values $meta-context $type)
                  (meta-resolve $meta-context $context
                    (signature-param $signature)
                    (typed-type $typed-rhs)))
                (values $meta-context
                  (typed
                    (signature-apply $signature $typed-rhs)
                    (application $typed-lhs $typed-rhs)))))
            ((else $other)
              (values $meta-context
                (typed
                  (typed (type 0)
                    (application
                      (typed-type $typed-lhs)
                      (typed-type $typed-rhs)))
                  (application $typed-lhs $typed-rhs)))))))

      ((else $other)
        (throw elab $meta-context $context $other))))

  (define (meta-context-index $meta-context $hole)
    (-
      (length $meta-context)
      (hole-index $hole)
      1))

  (define (resolve-task $env $expected $actual)
    (task ($solutions $errors)
      (switch $expected
        ((native-type? $native-type)
          (switch $actual
            ((native-type? _)
              (values $solutions $errors $native-type))
            ((else $other)
              (values $solutions (push $errors "not native") nothing))))
        ((else _)
          (if (term=? $expected $actual)
            (values $solutions $errors $expected)
            (values $solutions (push $errors "type error") nothing))))))

  (define (meta-resolve $meta-context $context $expected $actual)
    (switch $expected
      ((hole? $expected-hole)
        (lets
          ($index (meta-context-index $meta-context $expected-hole))
          (switch (list-ref $meta-context $index)
            ((unknown? _)
              (values
                (list-set $meta-context $index $actual)
                $actual))
            ((else $term)
              (meta-resolve $meta-context $context $term $actual)))))
      ((else $expected-other)
        (switch $actual
          ((hole? $actual-hole)
            (meta-resolve $meta-context $context $actual-hole $expected-other))
          ((else $actual-other)
            (if (term=? $expected-other $actual-other)
              (values $meta-context $actual-other)
              (values $meta-context
                (mismatch
                  (expected $expected-other)
                  (actual $actual-other)))))))))

  (define (infer $meta-context $context $term)
    (switch $term
      ((typed? $typed)
        (values $meta-context (typed-type $typed)))
      ((else $other)
        (throw infer $meta-context $context $term))))

  (define (cast $meta-context $context $type $term)
    (todo))

  (define (evaluate $meta-context $context $term)
    (switch $term
      ((evaluated? $evaluated)
        $evaluated)
      ((native? $native)
        (evaluated $native))
      ((native-application? $native-application)
        (lets
          ($procedure
            (native-application-procedure $native-application))
          ($evaluated-args
            (map
              (partial evaluate $meta-context $context)
              (native-application-args $native-application)))
          ($typed-args (map evaluated-ref $evaluated-args))
          ($evaluated-args (map typed-ref $typed-args))
          ($args (map evaluated-ref $evaluated-args))
          (evaluated
            (if (for-all native? $args)
              (native (apply $procedure (map native-ref $args)))
              (native-application $procedure $evaluated-args)))))
      ((procedure? $procedure)
        (evaluated
          (lambda ($0)
            (evaluate $meta-context $context
              ($procedure $0)))))
      ((application? $application)
        (lets
          ($evaluated-lhs
            (evaluate $meta-context $context (application-lhs $application)))
          ($evaluated-rhs
            (evaluate $meta-context $context (application-rhs $application)))
          (switch (evaluated-ref (typed-ref (evaluated-ref $evaluated-lhs)))
            ((procedure? $procedure)
              ($procedure $evaluated-rhs))
            ((else _)
              (evaluated (application $evaluated-lhs $evaluated-rhs))))))
      ((typed? $typed)
        (evaluated
          (typed
            (typed-type $typed)
            (evaluate $meta-context $context (typed-ref $typed)))))
      ; TODO
      ((else $other)
        (evaluated $other))))

  (define-rule-syntax (check-task=? in out)
    (check
      (equal?
        (task->datum in)
        (task->datum out))))

  (define-rules-syntax
    ((check-elabs in out)
      (check-elabs '() '() in out))
    ((check-elabs meta-context context in out)
      (check-term-datum=?
        (lets
          ((values $meta-context $typed)
            (elab meta-context context in))
          $typed)
        out)))

  (define-rule-syntax (check-elab-task=? in out)
    (check-task=? (elab-task empty-env in) out))

  (define-rules-syntax
    ((check-evaluates in out)
      (check-evaluates '() '() in out))
    ((check-evaluates meta-context context in out)
      (check-term-datum=?
        (evaluate meta-context context in)
        out)))
)
