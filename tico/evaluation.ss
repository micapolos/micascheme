(library (tico evaluation)
  (export
    evaluation-application
    evaluation-struct
    evaluation-args-application-opt
    evaluation-promote)
  (import
    (micascheme)
    (tico constant)
    (tico variable)
    (tico parameter)
    (tico datum))

  ;(enum (evaluation constant variable parameter))

  (define (evaluation-application $target $args)
    (lets
      ($evaluations (cons $target $args))
      (cond
        ((for-all constant? $evaluations)
          (constant-application $target $args))
        ((exists parameter? $evaluations)
          (parameter))
        (else
          (lets
            ($variables (filter variable? $evaluations))
            ($parameters (ensure null? (filter parameter? $evaluations)))
            ($variable (variable-flatten $variables))
            (variable (variable-index $variable)))))))

  (define (evaluation-struct $name $evaluations)
    (cond
      ((for-all constant? $evaluations)
        (constant-struct $name $evaluations))
      ((exists parameter? $evaluations)
        (parameter))
      (else
        (lets
          ($variables (filter variable? $evaluations))
          ($parameters (ensure null? (filter parameter? $evaluations)))
          ($variable (variable-flatten $variables))
          (variable (variable-index $variable))))))

  (define (evaluation-args-application-opt $target $args)
    (lets
      ($evaluations (list $target $args))
      (cond
        ((for-all constant? $evaluations) #f)
        ((exists parameter? $evaluations) (parameter))
        (else
          (lets
            ($variables (filter variable? $evaluations))
            ($variable (variable-flatten $variables))
            (variable (variable-index $variable)))))))

  (define (evaluation-promote $evaluation $arity)
    (switch-exclusive $evaluation
      ((constant? $constant) #f)
      ((variable? $variable) (variable-promote $variable $arity))
      ((parameter? $parameter) $parameter)))
)
