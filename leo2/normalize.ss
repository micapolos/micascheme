(library (leo2 normalize)
  (export normalize)
  (import (leo2 base) (leo2 term))

  (define (normalize $env $term)
    (switch $term
      ((native? $native) $native)
      ((type? $type) $type)
      ((variable? $variable)
        (switch (list-ref $env (variable-index $variable))
          ((hole? $hole) $variable)
          ((else $other) $other)))
      ((abstraction? $abstraction)
        (abstraction
          (normalize
            $env
            (abstraction-param $abstraction))
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
      ((application $application)
        (term-apply $env
          (normalize $env (application-lhs $application))
          (normalize $env (application-rhs $application))))
      ((recursive? $recursive)
        (recursive
          (normalize
            (push $env hole)
            (recursive-body $recursive))))
      ((branch? $branch)
        (switch (normalize $env (branch-condition $branch))
          ((boolean? $boolean)
            (normalize
              $env
              ((if $boolean branch-consequent branch-alternate) $branch)))
          ((else $condition)
            (branch
              $condition
              (normalize
                (push $env $condition)
                (branch-motive $branch))
              (normalize $env (branch-consequent $branch))
              (normalize $env (branch-alternate $branch))))))))

  (define (term-apply $env $lhs $rhs)
    (switch $lhs
      ((recursive? $recursive)
        (term-apply
          (push $env $recursive)
          (recursive-body $recursive)
          $rhs))
      ((abstraction? $abstraction)
        (normalize
          (push $env $rhs)
          (abstraction-body $abstraction)))
      ((abstraction-type? $abstraction-type)
        (normalize
          (push $env $rhs)
          (abstraction-type-body $abstraction-type)))
      ((native? $native)
        (switch (native-arity $native)
        ((zero? _)
          (application $native $rhs))
        ((one? _)
          (native
            0
            ((native-value $native) $rhs)
            (list)))
        ((else $arity)
          (native
            (- $arity 1)
            (native-value $native)
            (push (native-args $native) $rhs)))))
      ((else $lhs)
        (application $lhs $rhs))))
)
