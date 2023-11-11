(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation
    test-compilation

    literal->compilation
    datum->compilation
    datum->constant-compilation
    variable-compilation

    compilation-top-level-datum
    compilation-value

    compilation->generate-dependency-opt
    generate-parameter-compilation

    compilation-application
    compilation-abstraction
    compilation-struct
    compilation-ref

    compilation-parameter
    compilation-variable)
  (import
    (micascheme)
    (tico constant)
    (tico constant)
    (tico variable)
    (tico global)
    (tico dependency)
    (tico packet)
    (tico datum)
    (tico evaluation)
    (tico parameter))

  (data (compilation datum evaluation))

  (define-syntax-rule (test-compilation $name)
    (datum->compilation (quote $name)))

  (define (literal->compilation $literal)
    (compilation
      (literal->datum $literal)
      (constant $literal)))

  (define (datum->compilation $datum)
    (compilation $datum (global)))

  (define (datum->constant-compilation $datum)
    (compilation $datum (datum->constant $datum)))

  (define (compilation-top-level-datum $compilation)
    (lets-datum
      (evaluation-lets-datums (compilation-evaluation $compilation))
      (compilation-datum $compilation)))

  (define (compilation-value $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (constant-value $constant))
      ((global? $global)
        (datum->value (compilation-datum $compilation)))
      ((else _)
        (throw compilation-value $compilation))))

  (define (compilation->generate-dependency-opt $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (dependency
          (generate-symbol)
          (packet
            (compilation-datum $compilation)
            (constant-value $constant))))
      ((else _) #f)))

  (define (compilation->datum-dependencies $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        (lets
          ($symbol (generate-symbol))
          (cons $symbol
            (stack
              (dependency $symbol
                (packet
                  (compilation-datum $compilation)
                  (constant-value $constant)))))))
      ((global? $global) (stack))
      ((variable? $variable)
        (cons
          (compilation-datum $compilation)
          (variable-dependencies $variable)))
      ((parameter? $other)
        (throw compilation->datum-variable $other))))

  (define (generate-parameter-compilation)
    (compilation (generate-symbol) (parameter)))

  (define (compilation-parameter $compilation)
    (compilation
      (generate-symbol)
      (switch (compilation-evaluation $compilation)
        ((constant? $constant)
          $constant)
        ((global? $global)
          $global)
        ((variable? $variable)
          (parameter))
        ((parameter? $parameter)
          (throw compilation-parameter $compilation)))))

  (define (compilation-variable $compilation $index)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        $compilation)
      ((global? $global)
        $compilation)
      ((variable? $variable)
        (throw compilation-variable $compilation))
      ((parameter? $parameter)
        (compilation
          (compilation-datum $compilation)
          (variable $index (stack))))))

  (define (variable-compilation $datum $index)
    (compilation $datum (variable $index (stack))))

  (define (compilation-constantize $compilation)
    (switch (compilation-evaluation $compilation)
      ((global? $global)
        (datum->constant-compilation
          (compilation-datum $compilation)))
      ((else $other) $compilation)))

  (define (compilation-application $target $args)
    (compilation
      (datum-application
        (compilation-datum $target)
        (map compilation-datum $args))
      (evaluation-application
        (compilation-evaluation (compilation-constantize $target))
        (map compilation-evaluation (map compilation-constantize $args))
        (lambda ()
          (filter-opts
            (map compilation->generate-dependency-opt
              (reverse (cons $target $args))))))))

  (define (compilation-abstraction $param-compilations $body-compilation)
    (switch (compilation-evaluation $body-compilation)
      ((constant? $constant)
        (compilation
          (datum-abstraction
            (map compilation-datum $param-compilations)
            (compilation-datum $body-compilation))
          (constant-abstraction
            (length $param-compilations)
            $constant)))
      ((variable? $variable)
        (switch (variable-promote $variable (length $param-compilations))
          ((variable? $variable)
            (compilation
              (datum-abstraction
                (map compilation-datum $param-compilations)
                (compilation-datum $body-compilation))
              $variable))
          ((false? _)
            (datum->compilation
              (dependencies-lets
                (variable-dependencies $variable)
                (datum-abstraction
                  (map compilation-datum $param-compilations)
                  (compilation-datum $body-compilation)))))))
      ((parameter? $parameter)
        (throw compilation-abstraction $param-compilations $body-compilation))))

  (define (compilation-struct $name $compilations)
    (lets
      ($datums (map compilation-datum $compilations))
      ($evaluations (map compilation-evaluation $compilations))
      ($constants (filter constant? $evaluations))
      ($variables (filter variable? $evaluations))
      ($parameters (ensure null? (filter parameter? $evaluations)))
      (cond
        ((for-all constant? $evaluations)
          (compilation
            (datum-struct $name $datums)
            (constant-struct $name $constants)))
        (else
          (lets
            ($datum-dependencies-list
              (reverse (map compilation->datum-dependencies (reverse $compilations))))
            ($datums (map car $datum-dependencies-list))
            ($dependencies-list (map cdr $datum-dependencies-list))
            (compilation
              (datum-struct $name $datums)
              (variable
                (variable-index-flatten (map variable-index $variables))
                (dependencies-flatten $dependencies-list))))))))

  (define (compilation-ref $arity $target $index)
    (compilation
      (datum-ref $arity (compilation-datum $target) $index)
      (switch (compilation-evaluation $target)
        ((constant? $constant)
          (constant-ref $arity $constant $index))
        ((variable? $variable)
          $variable)
        ((parameter? $parameter)
          (throw compilation-ref $parameter)))))
)
