(library (tico compilation)
  (export
    compilation compilation? compilation-datum compilation-evaluation

    literal->compilation
    datum->compilation

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
    (tico dependency)
    (tico packet)
    (tico datum)
    (tico evaluation)
    (tico parameter))

  (data (compilation datum evaluation))

  (define (literal->compilation $literal)
    (compilation $literal (constant $literal)))

  (define (datum->compilation $datum)
    (compilation $datum (datum->constant $datum)))

  (define (compilation-top-level-datum $compilation)
    (lets-datum
      (evaluation-lets-datums (compilation-evaluation $compilation))
      (compilation-datum $compilation)))

  (define (compilation-value $compilation)
    (evaluation-value
      (compilation-evaluation $compilation)))

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
      ((variable? $variable)
        (cons
          (compilation-datum $compilation)
          (variable-dependencies $variable)))
      ((parameter? $other)
        (throw compilation->datum-variable $other))))

  (define (generate-parameter-compilation)
    (compilation (generate-symbol) (parameter)))

  (define (compilation-parameter $compilation)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        $compilation)
      ((variable? $variable)
        (generate-parameter-compilation))
      ((parameter? $parameter)
        (throw compilation-parameter $compilation))))

  (define (compilation-variable $compilation $index)
    (switch (compilation-evaluation $compilation)
      ((constant? $constant)
        $compilation)
      ((variable? $variable)
        (throw compilation-variable $compilation))
      ((parameter? $parameter)
        (compilation
          (compilation-datum $compilation)
          (variable $index (stack))))))

  (define (compilation-application $target $args)
    (compilation
      (datum-application
        (compilation-datum $target)
        (map compilation-datum $args))
      (evaluation-application
        (compilation-evaluation $target)
        (map compilation-evaluation $args)
        (lambda ()
          (filter-opts
            (map compilation->generate-dependency-opt
              (reverse (cons $target $args))))))))

  (define (compilation-abstraction $param-symbols $body-compilation)
    (compilation
      (datum-abstraction
        $param-symbols
        (compilation-datum $body-compilation))
      (evaluation-abstraction
        (length $param-symbols)
        (compilation-evaluation $body-compilation)
        (lambda () (compilation-datum $body-compilation)))))

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
