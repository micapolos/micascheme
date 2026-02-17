(library (leo2 normalize)
  (export
    hole hole?
    normalized normalized? normalized-value
    closure closure? closure-env closure-body
    normalize)
  (import (leo2 base) (leo2 term))

  (data hole)
  (data (normalized value))
  (data (closure env body))

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

  (define (shift $threshold $term)
    (switch-exhaustive $term
      ((normalized? $normalized)
        (normalized
          (shift
            $threshold
            (normalized-value $term))))
      ((native? $native)
        $native)
      ((native-application? $native-application)
        (native-application
          (native-application-procedure $native-application)
          (map
            (partial shift $threshold)
            (native-application-args $native-application))))
      ((variable? $variable)
        (lets
          ($index (variable-index $variable))
          (if (>= $index $threshold)
            (variable (+ $index 1))
            $variable)))
      ((abstraction? $abstraction)
        (abstraction
          (shift
            (+ $threshold 1)
            (abstraction-body $abstraction))))
      ((abstraction-type? $abstraction-type)
        (abstraction-type
          (shift
            $threshold
            (abstraction-type-param $abstraction-type))
          (shift
            (+ $threshold 1)
            (abstraction-type-body $abstraction-type))))
      ((recursive? $recursive)
        (recursive
          (shift
            (+ $threshold 1)
            (recursive-body $recursive))))
      ((application? $application)
        (application
          (shift $threshold (application-lhs $application))
          (shift $threshold (application-rhs $application))))
      ((branch? $branch)
        (branch
          (shift $threshold (branch-condition $branch))
          (shift $threshold (branch-consequent $branch))
          (shift $threshold (branch-alternate $branch))))))
)
