(library (leo2 normalize)
  (export normalize)
  (import
    (leo2 base)
    (leo2 term))

  (define (env-ref $env $variable)
    (switch (list-ref $env (variable-index $variable))
      ((hole? _) $variable)
      ((else $other) $other)))

  (define (normalize $env $term)
    (switch-exhaustive $term
      ((normalized? $normalized)
        $normalized)
      ((native? $native)
        (normalized $native))
      ((native-application? $native)
        (lets
          ($args
            (map
              (partial normalize $env)
              (native-application-args $native)))
          (if (for-all normalized? $args)
            (normalized
              (native
                (apply
                  (native-application-procedure $native)
                  (map (dot native-value normalized-value) $args))))
            (native-application
              (native-application-procedure $native)
              $args))))
      ((type? $type)
        (normalized $type))
      ((variable? $variable)
        (env-ref $env $variable))
      ((abstraction? $abstraction)
        (abstraction
          (normalize
            (push $env hole)
            (abstraction-body $abstraction))))
      ((abstraction-type? $abstraction-type)
        (abstraction-type
          (normalize
            $env
            (abstraction-type-param $abstraction-type))
          (normalize
            (push $env hole)
            (abstraction-type-body $abstraction-type))))
      ((application? $application)
        (term-apply $env
          (normalize $env (application-lhs $application))
          (normalize $env (application-rhs $application))))
      ((recursive? $recursive)
        (recursive
          (normalize
            (push $env hole)
            (recursive-body $recursive))))
      ((branch? $branch)
        (lets
          ($condition (normalize $env (branch-condition $branch)))
          (if (normalized? $condition)
            (normalize
              $env
              (app
                (if (native-value (normalized-value $condition))
                  branch-consequent
                  branch-alternate)
                $branch))
            (branch
              $condition
              (normalize $env (branch-consequent $branch))
              (normalize $env (branch-alternate $branch))))))))

  (define (term-apply $env $lhs $rhs)
    (switch $lhs
      ((abstraction? $abstraction)
        (normalize
          (push $env $rhs)
          (abstraction-body $abstraction)))
      ((abstraction-type? $abstraction-type)
        (normalize
          (push $env $rhs)
          (abstraction-type-body $abstraction-type)))
      ((recursive? $recursive)
        (term-apply
          (push $env $recursive)
          (recursive-body $recursive)
          $rhs))
      ((else $other)
        (application $lhs $rhs))))
)
