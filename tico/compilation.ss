(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation
    test-compilation
    test-parameter-compilation
    test-compilation-scope

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

    empty-compilation-scope
    compilation-scope-push
    compilation-scope-ref
    compilation-scope-bindings
    compilation-scope
    test-compilation-scope)
  (import
    (micascheme)
    (tico constant)
    (tico constant)
    (tico variable)
    (tico datum)
    (tico evaluation)
    (tico parameter))

  (data (compilation datum evaluation))

  (define-syntax-rule (test-compilation $name)
    (datum->compilation (test-datum $name)))

  (define-syntax-rule (test-parameter-compilation $name)
    (compilation
      (test-parameter-datum $name)
      (parameter)))

  (define-syntax-rule (test-compilation-scope $name ...)
    (compilation-scope
      (test-compilation $name) ...))

  (define (literal->compilation $literal)
    (compilation
      (literal->datum $literal)
      (constant $literal)))

  (define (datum->compilation $datum)
    (compilation $datum (datum->constant $datum)))

  (define (compilation-binding-opt $compilation)
    (switch-opt (compilation-evaluation $compilation)
      ((constant? $constant)
        (cons
          (compilation-datum $compilation)
          (constant-value $constant)))))

  (define (bindings-datum->compilation $binding-compilations $datum)
    (compilation $datum
      (bindings-datum->constant
        (filter-opts (map compilation-binding-opt $binding-compilations))
        $datum)))

  (define (scope-datum->compilation $scope $datum)
    (compilation $datum
      (bindings-datum->constant
        (compilation-scope-bindings $scope)
        $datum)))

  (define (compilation-value $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (constant-value $constant))
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
        ((constant? $constant)
          $constant)
        ((variable? $variable)
          (parameter))
        ((parameter? $parameter)
          (parameter)))))

  (define (compilation-variable $compilation $index)
    (switch-exclusive (compilation-evaluation $compilation)
      ((constant? $constant)
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
          ; (compilation $datum
          ;   (constant
          ;     (call-with-values
          ;       (constant-value (compilation-evaluation $args))
          ;       (constant-value (compilation-evaluation $target)))))))))
          (bindings-datum->constant
            (compilation-scope-bindings $scope)
            $datum)))))

  (define (compilation-abstraction $scope $param-compilations $body-compilation)
    (switch-exclusive (compilation-evaluation $body-compilation)
      ((constant? $constant)
        (compilation
          (datum-abstraction
            (map compilation-datum $param-compilations)
            (compilation-datum $body-compilation))
          (constant-abstraction
            (length $param-compilations)
            $constant)))
      ((variable? $variable)
        (switch-exclusive (variable-promote $variable (length $param-compilations))
          ((variable? $variable)
            (compilation
              (datum-abstraction
                (map compilation-datum $param-compilations)
                (compilation-datum $body-compilation))
              $variable))
          ((false? _)
            (scope-datum->compilation
              $scope
              (datum-abstraction
                (map compilation-datum $param-compilations)
                (compilation-datum $body-compilation))))))
      ((parameter? $parameter)
        (compilation
          (datum-abstraction
            (map compilation-datum $param-compilations)
            (compilation-datum $body-compilation))
          $parameter))))

  (define (compilation-struct $name $compilations)
    (lets
      ($datums (map compilation-datum $compilations))
      ($evaluations (map compilation-evaluation $compilations))
      ($constants (filter constant? $evaluations))
      ($variables (filter variable? $evaluations))
      ($parameters (filter parameter? $evaluations))
      (cond
        ((for-all constant? $evaluations)
          (compilation
            (datum-struct $name $datums)
            (constant-struct $name $constants)))
        (else
          (lets
            ($datums (map compilation-datum $compilations))
            (compilation
              (datum-struct $name $datums)
              (cond
                ((null? $parameters)
                  (variable
                    (variable-index-flatten (map variable-index $variables))))
                (else (parameter)))))))))

  (define (compilation-args $scope $compilations)
    (lets
      ($datum (datum-args (map compilation-datum $compilations)))
      ($evaluations (map compilation-evaluation $compilations))
      (cond
        ((for-all constant? $evaluations)
          (compilation $datum
            (constant
              (lambda ()
                (apply values
                  (map constant-value $evaluations))))))
          ; (scope-datum->compilation $scope $datum))
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
        ((constant? $constant)
          (constant-ref $arity $constant $index))
        ((variable? $variable)
          $variable)
        ((parameter? $parameter)
          (throw compilation-ref $parameter)))))

  (define (empty-compilation-scope)
    (compilation (stack) (stack)))

  (define (compilation-scope-push $compilation-scope $compilation)
    (compilation
      (push
        (compilation-datum $compilation-scope)
        (compilation-datum $compilation))
      (push
        (compilation-evaluation $compilation-scope)
        (compilation-evaluation $compilation))))

  (define (compilation-scope-ref $compilation-scope $index)
    (compilation-variable
      (compilation
        (list-ref (compilation-datum $compilation-scope) $index)
        (list-ref (compilation-evaluation $compilation-scope) $index))
      $index))

  (define (compilation-scope-bindings $compilation-scope)
    (filter-opts
      (map
        (lambda ($datum $evaluation)
          (switch-opt $evaluation
            ((constant? $constant)
              (cons $datum (constant-value $constant)))))
        (compilation-datum $compilation-scope)
        (compilation-evaluation $compilation-scope))))

  (define-syntax-rule (compilation-scope $item ...)
    (fold-left
      compilation-scope-push
      (empty-compilation-scope)
      (list $item ...)))
)
