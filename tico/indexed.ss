(library (tico indexed)
  (export
    evaluated evaluated? evaluated-value
    variable variable? variable-index
    abstraction abstraction? abstraction-arity abstraction-body
    application application? application-target application-args
    expansion expansion? expansion-body
    thunk thunk? thunk-bindings thunk-term
    term-evaluate)
  (import (micascheme))

  (data (evaluated value))
  (data (variable index))
  (data (abstraction arity body))
  (data (application target args))
  (data (hole))
  (data (expansion body))
  (data (thunk bindings term))

  (define (term-evaluate $term)
    (value->term (term->value $term)))

  (define (term->value $term)
    (bindings-term->value (stack) $term))

  (define (bindings-term->value $bindings $term)
    (switch $term
      ((evaluated? $evaluated) $evaluated)
      ((variable? $variable)
        (switch (list-ref-opt $bindings (variable-index $variable))
          ((hole? _) $variable)
          ((false? _) $variable)
          ((else $other) $other)))
      ((abstraction? $abstraction)
        (thunk $bindings
          (abstraction
            (abstraction-arity $abstraction)
            (bindings-term->value
              (iterate
                (lambda ($bindings) (push $bindings (hole)))
                $bindings
                (abstraction-arity $abstraction))
              (abstraction-body $abstraction)))))
      ((application? $application)
        (term-apply
          (bindings-term->value $bindings (application-target $application))
          (map (partial bindings-term->value $bindings) (application-args $application))))
      ((expansion? $expansion)
        (bindings-term->value $bindings
          (evaluated-value
            (bindings-term->value $bindings (expansion-body $expansion)))))
      ((else $other) (throw not-term $other))))

  (define (term-apply $target $args)
    (switch $target
      ((thunk? $thunk)
        (bindings-term->value
          (push-list (thunk-bindings $thunk) $args)
          (abstraction-body (thunk-term $thunk))))
      ((evaluated? $evaluated)
        (cond
          ((for-all evaluated? $args)
            (evaluated
              (apply
                (evaluated-value $evaluated)
                (map evaluated-value $args))))
          (else
            (application $evaluated $args))))
      ((else $other)
        (application $other $args))))

  (define (value->term $value)
    (switch $value
      ((thunk? $thunk)
        (lets
          ($bindings (thunk-bindings $thunk))
          ($arity (length $bindings))
          ($term (thunk-term $thunk))
          (case $arity
            ((0) $term)
            (else
              (application
                (abstraction $arity $term)
                (reverse $bindings))))))
      ((evaluated? $evaluated) $evaluated)
      ((variable? $variable) $variable)
      ((application? $application) $application)
      ((abstraction? $abstraction) $abstraction)
      ((else $other) (throw not-term $other))))
)
