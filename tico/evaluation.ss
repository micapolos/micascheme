(library (tico evaluation)
  (export
    evaluation-application
    evaluation-lets-datums)
  (import
    (micascheme)
    (tico constant)
    (tico variable)
    (tico parameter)
    (tico dependency)
    (tico datum)
    (tico global))

  ;(enum (evaluation constant variable parameter))

  (define (evaluation-lets-datums $evaluation)
    (switch $evaluation
      ((variable? $variable)
        (variable-lets-datums $variable))
      ((else _)
        (list))))

  (define (evaluation-application $target $args $constant-dependencies-fn)
    (lets
      ($evaluations (cons $target $args))
      (cond
        ((for-all constant? $evaluations)
          (constant-application $target $args))
        (else
          (lets
            ($variables (filter variable? $evaluations))
            ($globals (ensure null? (filter global? $evaluations)))
            ($parameters (ensure null? (filter parameter? $evaluations)))
            ($variable (variable-flatten $variables))
            (variable
              (variable-index $variable)
              (dependencies-flatten
                (list
                  (app $constant-dependencies-fn)
                  (variable-dependencies $variable)))))))))
)
