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

    constant-compilation
    variable-compilation
    parameter-compilation

    generate-parameter-compilation

    compilation-arity-datum
    compilation-application
    compilation-abstraction
    compilation-args
    compilation-struct
    compilation-ref

    compilation-parameter
    compilation-parameters
    compilation-argument
    compilation-datum-argument
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
    (tico constant)
    (tico constant)
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

  (function (compilation-arity-datum $compilation)
    `(
      ,(arity-value (compilation-arity $compilation))
      ,(compilation-datum $compilation)))

  (function (literal->compilation $literal)
    (datum->compilation
      (literal->datum $literal)))

  (function (datum->compilation $datum)
    (scope-datum->compilation
      (empty-stack-compilation)
      $datum))

  (function (bindings-datum->compilation $binding-compilations $datum)
    (scope-datum->compilation
      (apply stack-compilation $binding-compilations)
      $datum))

  (function (scope-datum->compilation $scope $datum)
    (lets
      ($constant
        (bindings-datum->constant
          (stack-compilation-bindings $scope)
          $datum))
      (compilation
        (constant-arity $constant)
        $datum
        $constant)))

  (function (compilation-binding-opt $compilation)
    (switch-opt (compilation-evaluation $compilation)
      ((constant? $constant)
        (cons
          (compilation-datum $compilation)
          (constant-value $constant)))))

  (function (compilation-value $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (constant-value $constant))
      ((else $other)
        (throw compilation-value $compilation))))

  (function (parameter-compilation $symbol)
    (compilation (arity 1) $symbol (parameter)))

  (function (constant-compilation $datum $constant)
    (compilation
      (constant-arity $constant)
      $datum
      $constant))

  (function (generate-parameter-compilation)
    (parameter-compilation (generate-symbol)))

  (function (compilation-parameter $compilation)
    (lets
      ((compilation $arity $datum $evaluation) $compilation)
      (compilation
        $arity
        (datum-parameter $arity)
        (evaluation-parameter $evaluation))))

  (function (compilation-parameters $compilation)
    (lets
      ((compilation $arity $datum $evaluation) $compilation)
      (map compilation
        (make-list (arity-value $arity) (arity 1))
        (datum-parameters $arity)
        (evaluation-parameters $arity $evaluation))))

  (function (compilation-variable $compilation $index)
    (switch-exclusive (compilation-evaluation $compilation)
      ((constant? $constant)
        $compilation)
      ((variable? $variable)
        (throw compilation-variable $compilation))
      ((parameter? $parameter)
        (variable-compilation
          (compilation-datum $compilation) $index))))

  (function (variable-compilation $datum $index)
    (compilation (arity 1) $datum (variable $index)))

  (function (compilation-application $arity $target $args)
    (lets
      ($arities (map compilation-arity $args))
      (compilation
        $arity
        (cond
          ((for-all arity-single? $arities)
            (datum-application
              (compilation-datum $target)
              (map compilation-datum $args)))
          (else
            (datum-values-application
              (compilation-datum $target)
              (map compilation-arity-datum $args))))
        (evaluation-application
          (compilation-evaluation $target)
          (map compilation-evaluation $args)))))

  (function (compilation-abstraction $scope $param-compilations $body-compilations)
    (lets
      ($datum
        (datum-abstraction
          (map compilation-datum $param-compilations)
          (values-datum (map compilation-datum $body-compilations))))
      (compilation
        (arity 1)
        $datum
        (lets
          ($evaluation-opt
            (evaluations-combine
              (map compilation-evaluation $body-compilations)))
          ($evaluation-opt
            (and $evaluation-opt
              (evaluation-promote $evaluation-opt
                (length $param-compilations))))
          (or $evaluation-opt
            (bindings-datum->constant
              (stack-compilation-bindings $scope)
              $datum))))))

  (function (compilation-struct $name $compilations)
    (compilation
      (arity 1)
      (datum-struct $name (map compilation-datum $compilations))
      (evaluation-struct $name (map compilation-evaluation $compilations))))

  (function (compilation-args $compilations)
    (lets
      ($datum (datum-args (map compilation-datum $compilations)))
      ($evaluations (map compilation-evaluation $compilations))
      (cond
        ((for-all constant? $evaluations)
          (constant-compilation
            $datum
            (constant (map constant-value $evaluations))))
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

  (function (compilation-ref $arity $target $index)
    (compilation
      (arity 1)
      (datum-ref $arity (compilation-datum $target) $index)
      (switch-exclusive (compilation-evaluation $target)
        ((constant? $constant)
          (constant-ref $arity $constant $index))
        ((variable? $variable)
          $variable)
        ((parameter? $parameter)
          (throw compilation-ref $parameter)))))

  (function (empty-stack-compilation)
    (compilation (arity 0) (stack) (stack)))

  (function (stack-compilation-push $stack-compilation $compilation)
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

  (function (stack-compilation-ref $stack-compilation $index)
    (compilation-variable
      (compilation
        (arity 1)
        (list-ref (compilation-datum $stack-compilation) $index)
        (list-ref (compilation-evaluation $stack-compilation) $index))
      $index))

  (function (stack-compilation-bindings $stack-compilation)
    (filter-opts
      (map
        (lambda ($datum $evaluation)
          (switch-opt $evaluation
            ((constant? $constant)
              (cons $datum (constant-value $constant)))))
        (compilation-datum $stack-compilation)
        (compilation-evaluation $stack-compilation))))

  (function (stack-compilation . $compilations)
    (fold-left
      stack-compilation-push
      (empty-stack-compilation)
      $compilations))

  (function (compilation-definition->datum-definition $compilation-definition)
    (definition-map compilation-datum $compilation-definition))

  (function (compilation-definitions-do $compilation-definitions $body-compilation)
    (compilation
      (arity 1)
      `(let
        ,(datum-definitions-let-entries (map compilation-definition->datum-definition $compilation-definitions))
        ,(compilation-datum $body-compilation))
      (compilation-evaluation $body-compilation)))

  (function (compilation-argument $compilation)
    (argument
      (compilation-parameters $compilation)
      $compilation))

  (function (compilation-datum-argument $compilation-argument)
    (lets
      ((argument $key-compilation $value-compilation) $compilation-argument)
        (argument
          (compilation-datum $key-compilation)
          (compilation-datum $value-compilation))))
)
