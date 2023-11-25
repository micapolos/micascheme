(library (tico evaluation)
  (export
    evaluations-combine
    evaluation-application
    evaluation-struct
    evaluation-args-application-opt
    evaluation-promote)
  (import
    (micascheme)
    (tico argument)
    (tico variable)
    (tico parameter)
    (tico datum))

  ;(enum (evaluation argument variable parameter))

  (define (evaluations-combine $evaluations)
    (cond
      ((for-all argument? $evaluations) #f)
      ((exists parameter? $evaluations) (parameter))
      (else (variable-flatten (filter variable? $evaluations)))))

  (define (evaluation-application $target $args)
    (or
      (evaluations-combine (cons $target $args))
      (argument-application $target $args)))

  (define (evaluation-struct $name $evaluations)
    (or
      (evaluations-combine $evaluations)
      (argument-struct $name $evaluations)))

  (define (evaluation-args-application-opt $target $args)
    (lets
      ($evaluations (list $target $args))
      (cond
        ((for-all argument? $evaluations) #f)
        ((exists parameter? $evaluations) (parameter))
        (else
          (lets
            ($variables (filter variable? $evaluations))
            ($variable (variable-flatten $variables))
            (variable (variable-index $variable)))))))

  (define (evaluation-promote $evaluation $arity)
    (switch-exclusive $evaluation
      ((argument? $argument) #f)
      ((variable? $variable) (variable-promote $variable $arity))
      ((parameter? $parameter) $parameter)))
)
