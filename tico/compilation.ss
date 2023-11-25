(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation
    test-compilation
    test-parameter-compilation
    test-stack-compilation

    literal->compilation
    datum->compilation
    bindings-datum->compilation
    scope-datum->compilation
    variable-compilation

    compilation-value

    parameter-compilation
    generate-parameter-compilation

    compilation-application
    compilation-args-application
    compilation-abstraction
    compilation-args
    compilation-struct
    compilation-ref

    compilation-parameter
    compilation-variable
    compilation-definitions-do

    empty-stack-compilation
    stack-compilation-push
    stack-compilation-ref
    stack-compilation-bindings
    stack-compilation
    test-stack-compilation)
  (import
    (micascheme)
    (tico arity)
    (tico argument)
    (tico argument)
    (tico variable)
    (tico datum)
    (tico evaluation)
    (tico parameter)
    (tico definition))

  (data (compilation datum evaluation))

  (define-syntax-rule (test-compilation $name)
    (datum->compilation (test-datum $name)))

  (define-syntax-rule (test-parameter-compilation $name)
    (compilation
      (test-parameter-datum $name)
      (parameter)))

  (define-syntax-rule (test-stack-compilation $name ...)
    (stack-compilation
      (test-compilation $name) ...))

  (define (literal->compilation $literal)
    (compilation
      (literal->datum $literal)
      (argument $literal)))

  (define (datum->compilation $datum)
    (scope-datum->compilation
      (empty-stack-compilation)
      $datum))

  (define (bindings-datum->compilation $binding-compilations $datum)
    (compilation $datum
      (bindings-datum->argument
        (filter-opts (map compilation-binding-opt $binding-compilations))
        $datum)))

  (define (scope-datum->compilation $scope $datum)
    (compilation $datum
      (bindings-datum->argument
        (stack-compilation-bindings $scope)
        $datum)))

  (define (compilation-binding-opt $compilation)
    (switch-opt (compilation-evaluation $compilation)
      ((argument? $argument)
        (cons
          (compilation-datum $compilation)
          (argument-value $argument)))))

  (define (compilation-value $compilation)
    (switch (compilation-evaluation $compilation)
      ((argument? $argument)
        (argument-value $argument))
      ((else $other)
        (throw compilation-value $compilation))))

  (define (parameter-compilation $symbol)
    (compilation $symbol (parameter)))

  (define (generate-parameter-compilation)
    (parameter-compilation (generate-symbol)))

  (define (compilation-parameter $compilation)
    (compilation
      (generate-symbol)
      (switch-exclusive (compilation-evaluation $compilation)
        ((argument? $argument)
          $argument)
        ((variable? $variable)
          (parameter))
        ((parameter? $parameter)
          (parameter)))))

  (define (compilation-variable $compilation $index)
    (switch-exclusive (compilation-evaluation $compilation)
      ((argument? $argument)
        $compilation)
      ((variable? $variable)
        (throw compilation-variable $compilation))
      ((parameter? $parameter)
        (compilation
          (compilation-datum $compilation)
          (variable $index)))))

  (define (variable-compilation $datum $index)
    (compilation $datum (variable $index)))

  (define (compilation-application $target $args)
    (compilation
      (datum-application
        (compilation-datum $target)
        (map compilation-datum $args))
      (evaluation-application
        (compilation-evaluation $target)
        (map compilation-evaluation $args))))

  (define (compilation-args-application $scope $target $args)
    (lets
      ($datum
        (datum-args-application
        (compilation-datum $target)
        (compilation-datum $args)))
      ($evaluation-opt
        (evaluation-args-application-opt
          (compilation-evaluation $target)
          (compilation-evaluation $args)))
      (compilation $datum
        (or $evaluation-opt
          (bindings-datum->argument
            (stack-compilation-bindings $scope)
            $datum)))))

  (define (compilation-abstraction $scope $param-compilations $body-compilation)
    (lets
      ($datum
        (datum-abstraction
          (map compilation-datum $param-compilations)
          (compilation-datum $body-compilation)))
      (compilation
        $datum
        (or
          (evaluation-promote
            (compilation-evaluation $body-compilation)
            (length $param-compilations))
          (bindings-datum->argument
            (stack-compilation-bindings $scope)
            $datum)))))

  (define (compilation-struct $name $compilations)
    (compilation
      (datum-struct $name (map compilation-datum $compilations))
      (evaluation-struct $name (map compilation-evaluation $compilations))))

  (define (compilation-args $compilations)
    (lets
      ($datum (datum-args (map compilation-datum $compilations)))
      ($evaluations (map compilation-evaluation $compilations))
      (cond
        ((for-all argument? $evaluations)
          (compilation $datum
            (argument (map argument-value $evaluations))))
        (else
          (compilation $datum
            (cond
              ((null? (filter parameter? $evaluations))
                (variable
                  (variable-index-flatten
                    (map variable-index
                      (filter variable? $evaluations)))))
              (else (parameter))))))))

  (define (compilation-ref $arity $target $index)
    (compilation
      (datum-ref $arity (compilation-datum $target) $index)
      (switch-exclusive (compilation-evaluation $target)
        ((argument? $argument)
          (argument-ref $arity $argument $index))
        ((variable? $variable)
          $variable)
        ((parameter? $parameter)
          (throw compilation-ref $parameter)))))

  (define (empty-stack-compilation)
    (compilation (stack) (stack)))

  (define (stack-compilation-push $stack-compilation $compilation)
    (compilation
      (push
        (compilation-datum $stack-compilation)
        (compilation-datum $compilation))
      (push
        (compilation-evaluation $stack-compilation)
        (compilation-evaluation $compilation))))

  (define (stack-compilation-ref $stack-compilation $index)
    (compilation-variable
      (compilation
        (list-ref (compilation-datum $stack-compilation) $index)
        (list-ref (compilation-evaluation $stack-compilation) $index))
      $index))

  (define (stack-compilation-bindings $stack-compilation)
    (filter-opts
      (map
        (lambda ($datum $evaluation)
          (switch-opt $evaluation
            ((argument? $argument)
              (cons $datum (argument-value $argument)))))
        (compilation-datum $stack-compilation)
        (compilation-evaluation $stack-compilation))))

  (define (stack-compilation . $compilations)
    (fold-left
      stack-compilation-push
      (empty-stack-compilation)
      $compilations))

  (define (compilation-definition->datum-definition $compilation-definition)
    (definition-map compilation-datum $compilation-definition))

  (define (compilation-definitions-do $compilation-definitions $body-compilation)
    (compilation
      `(let
        ,(datum-definitions-let-entries (map compilation-definition->datum-definition $compilation-definitions))
        ,(compilation-datum $body-compilation))
      (compilation-evaluation $body-compilation)))
)
