(library (tico evaluation)
  (export
    evaluation-application)
  (import
    (micascheme)
    (tico constant)
    (tico variable)
    (tico parameter)
    (tico datum)
    (tico global))

  ;(enum (evaluation constant variable parameter))

  (define (evaluation-application $target $args)
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
              (variable-index $variable)))))))
)
