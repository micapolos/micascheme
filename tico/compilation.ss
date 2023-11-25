(library (tico compilation)
  (export
    compilation compilation? compilation-arity compilation-datum compilation-evaluation
    test-compilation
    test-parameter-compilation
    test-stack-compilation

    literal->compilation
    datum->compilation
    bindings-datum->compilation
    scope-datum->compilation

    compilation-value

    argument-compilation
    variable-compilation
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

  (data (compilation arity datum evaluation))

  (define-syntax-rule (test-compilation $name)
    (datum->compilation (test-datum $name)))

  (define-syntax-rule (test-parameter-compilation $name)
    (parameter-compilation
      (test-parameter-datum $name)))

  (define-syntax-rule (test-stack-compilation $name ...)
    (stack-compilation
      (test-compilation $name) ...))

  (define (literal->compilation $literal)
    (datum->compilation
      (literal->datum $literal)))

  (define (datum->compilation $datum)
    (scope-datum->compilation
      (empty-stack-compilation)
      $datum))

  (define (bindings-datum->compilation $binding-compilations $datum)
    (scope-datum->compilation
      (apply stack-compilation $binding-compilations)
      $datum))

  (define (scope-datum->compilation $scope $datum)
    (lets
      ($argument
        (bindings-datum->argument
          (stack-compilation-bindings $scope)
          $datum))
      (compilation
        (argument-arity $argument)
        $datum
        $argument)))

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
    (compilation (arity 1) $symbol (parameter)))

  (define (argument-compilation $datum $argument)
    (compilation
      (argument-arity $argument)
      $datum
      $argument))

  (define (generate-parameter-compilation)
    (parameter-compilation (generate-symbol)))

  (define (compilation-parameter $compilation)
    (compilation
      (arity 1) ; TODO: Check it
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
        (variable-compilation
          (compilation-datum $compilation) $index))))

  (define (variable-compilation $datum $index)
    (compilation (arity 1) $datum (variable $index)))

  (define (compilation-application $target $args)
    (compilation
      (arity 1) ; TODO: Make it a parameter
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
      (compilation
        (arity 1)
        $datum
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
        (arity 1)
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
      (arity 1)
      (datum-struct $name (map compilation-datum $compilations))
      (evaluation-struct $name (map compilation-evaluation $compilations))))

  (define (compilation-args $compilations)
    (lets
      ($datum (datum-args (map compilation-datum $compilations)))
      ($evaluations (map compilation-evaluation $compilations))
      (cond
        ((for-all argument? $evaluations)
          (argument-compilation
            $datum
            (argument (map argument-value $evaluations))))
        (else
          (compilation
            (arity 1)
            $datum
            (cond
              ((null? (filter parameter? $evaluations))
                (variable
                  (variable-index-flatten
                    (map variable-index
                      (filter variable? $evaluations)))))
              (else (parameter))))))))

  (define (compilation-ref $arity $target $index)
    (compilation
      (arity 1)
      (datum-ref $arity (compilation-datum $target) $index)
      (switch-exclusive (compilation-evaluation $target)
        ((argument? $argument)
          (argument-ref $arity $argument $index))
        ((variable? $variable)
          $variable)
        ((parameter? $parameter)
          (throw compilation-ref $parameter)))))

  (define (empty-stack-compilation)
    (compilation (arity 0) (stack) (stack)))

  (define (stack-compilation-push $stack-compilation $compilation)
    (compilation
      (arity+
        (compilation-arity $stack-compilation)
        (compilation-arity $compilation))
      (push
        (compilation-datum $stack-compilation)
        (compilation-datum $compilation))
      (push
        (compilation-evaluation $stack-compilation)
        (compilation-evaluation $compilation))))

  (define (stack-compilation-ref $stack-compilation $index)
    (compilation-variable
      (compilation
        (arity 1)
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
      (arity 1)
      `(let
        ,(datum-definitions-let-entries (map compilation-definition->datum-definition $compilation-definitions))
        ,(compilation-datum $body-compilation))
      (compilation-evaluation $body-compilation)))
)
