(library (tico indexed)
  (export
    variable variable? variable-index
    abstraction abstraction? abstraction-body
    application application? application-target application-args
    thunk thunk? thunk-bindings thunk-term
    term-evaluate)
  (import (micascheme))

  (data (variable index))
  (data (abstraction body))
  (data (application target args))
  (data (evaluated value))
  (data (compiled evaluated-opt datum depth))
  (data (thunk bindings term))

  (define (term-evaluate $term)
    (value->term (term->value $term)))

  (define (term->value $term)
    (bindings-term->value (stack) $term))

  (define (bindings-term->value $bindings $term)
    (switch $term
      ((variable? $variable)
        (list-ref $bindings (variable-index $variable)))
      ((abstraction? $abstraction)
        (thunk $bindings $abstraction))
      ((application? $application)
        (term-apply
          (bindings-term->value $bindings (application-target $application))
          (map (partial bindings-term->value $bindings) (application-args $application))))
      ((else $other)
        $other)))

  (define (term-apply $target $args)
    (switch $target
      ((thunk? $thunk)
        (bindings-term->value
          (push-list (thunk-bindings $thunk) $args)
          (abstraction-body (thunk-term $thunk))))
      ((else $other)
        (apply $other $args))))

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
                (abstraction $term)
                (reverse $bindings))))))
      ((else $other) $other)))
)
