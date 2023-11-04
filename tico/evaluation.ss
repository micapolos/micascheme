(library (tico evaluation)
  (export
    evaluation-application)
  (import
    (micascheme)
    (tico constant)
    (tico variable)
    (tico dependency))

  (enum (evaluation constant variable))

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
)
