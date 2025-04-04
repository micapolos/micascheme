(library (tico evaluation)
  (export
    evaluations-combine
    evaluation-application
    evaluation-struct
    evaluation-promote
    evaluation-parameter
    evaluation-parameters)
  (import
    (micascheme)
    (tico arity)
    (tico constant)
    (tico variable)
    (tico parameter)
    (tico datum))

  ;(enum (evaluation constant variable parameter))

  (define (evaluations-combine $evaluations)
    (cond
      ((for-all constant? $evaluations) #f)
      ((exists parameter? $evaluations) (parameter))
      (else (variable-flatten (filter variable? $evaluations)))))

  (define (evaluation-application $target $args)
    (or
      (evaluations-combine (cons $target $args))
      (constant-application $target $args)))

  (define (evaluation-struct $name $evaluations)
    (or
      (evaluations-combine $evaluations)
      (constant-struct $name $evaluations)))

  (define (evaluation-promote $evaluation $arity)
    (switch-exhaustive $evaluation
      ((constant? $constant) #f)
      ((variable? $variable) (variable-promote $variable $arity))
      ((parameter? $parameter) $parameter)))

  (define (evaluation-parameter $evaluation)
    (switch-exhaustive $evaluation
      ((constant? $constant) $constant)
      ((variable? _) (parameter))
      ((parameter? _) (parameter))))

  (define (evaluation-parameters $arity $evaluation)
    (lets
      ((arity $size) $arity)
      (switch-exhaustive $evaluation
        ((constant? $constant)
          (constant-parameters $constant))
        ((variable? _)
          (make-list $size (parameter)))
        ((parameter? _)
          (make-list $size (parameter))))))
)
