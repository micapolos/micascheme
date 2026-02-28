(library (leo2 elab)
  (export
    elab
    check-elabs
    check-elab-throws)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum)
    (leo2 equal))

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
          (typed nothing $native)))

      ((native-application? $native-application)
        (lets
          ((values $meta-context $typed-args)
            (elab* $meta-context $context
              (native-application-args $native-application)))
          (values $meta-context
            (typed nothing
              (native-application
                (native-application-procedure $native-application)
                $typed-args)))))

      ((type? $type)
        (values $meta-context
          (typed
            (type (+ (type-depth $type) 1))
            $type)))

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
          ($meta-context (push $meta-context #f))
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
                (typed nothing
                (application $typed-lhs $typed-rhs)))))))

      ((else $other)
        (throw elab $meta-context $context $other))))

  (define (meta-context-index $meta-context $hole)
    (-
      (length $meta-context)
      (hole-index $hole)
      1))

  (define (meta-resolve $meta-context $context $expected $actual)
    (switch $expected
      ((hole? $expected-hole)
        (lets
          ($index (meta-context-index $meta-context $expected-hole))
          (switch (list-ref $meta-context $index)
            ((false? _)
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
            ; TODO: meta-resolve should handle all terms.
            (if (term=? $expected-other $actual-other)
              (values $meta-context $actual-other)
              (values $meta-context nothing)))))))

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
      ; TODO
      ((else $other)
        (evaluated $other))))

  (define-rules-syntax
    ((check-elabs in out)
      (check-elabs '() '() in out))
    ((check-elabs meta-context context in out)
      (check-term->datum=?
        (lets
          ((values $meta-context $typed)
          (elab meta-context context in)) $typed)
        out)))

  (define-rule-syntax (check-elab-throws in)
    (check (raises (elab '() '() in))))
)
