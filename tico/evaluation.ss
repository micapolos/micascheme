(library (tico evaluation)
  (export
    evaluation-application
    evaluation-abstraction
    evaluation-lets-datums
    evaluation-value)
  (import
    (micascheme)
    (tico constant)
    (tico variable)
    (tico dependency)
    (tico datum))

  (enum (evaluation constant variable))

  (define (evaluation-lets-datums $evaluation)
    (switch $evaluation
      ((constant? _)
        (list))
      ((variable? $variable)
        (variable-lets-datums $variable))))

  (define (evaluation-value $evaluation)
    (switch $evaluation
      ((constant? $constant)
        (constant-value $constant))
      ((variable? $variable)
        (throw evaluation-value $evaluation))))

  (define (evaluation-application $target $args $constant-dependencies-fn)
    (lets
      ($evaluations (cons $target $args))
      (cond
        ((for-all constant? $evaluations)
          (constant-application $target $args))
        (else
          (lets
            ($variables (filter variable? $evaluations))
            ($constants (filter constant? $evaluations))
            (variable
              (variables-index $variables)
              (tuple-dependencies
                (cons
                  (app $constant-dependencies-fn)
                  (map variable-dependencies $variables)))))))))

  (define (evaluation-abstraction $arity $body-evaluation $body-datum-fn)
    (switch $body-evaluation
      ((constant? $constant) $constant)
      ((variable? $variable)
        (or
          (variable-promote $variable $arity)
          (datum->constant
            (lets-datum
              (reverse (map dependency-lets-datum (variable-dependencies $variable)))
              (app $body-datum-fn)))))))
)
