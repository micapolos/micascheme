(library (tico layment)
  (export
    layment layment? layment-layout layment-compilation
    layment-not-empty?
    test-layment
    test-parameter-layment

    make-layment
    empty-layment
    literal->layment
    variable-layment
    layout-datum->layment
    bindings-layout-datum->layment

    layment-datum
    layment-value

    parameter-layment
    generate-parameter-layment
    layment-application
    layment-parameter
    layment-variable
    scope-layment-abstraction
    layment-abstraction
    layment-args
    layment-struct
    layment-ref

    empty-stack-layment
    stack-layment-push
    stack-layment-ref)
  (import
    (micascheme)
    (tico datum)
    (tico layout)
    (tico constant)
    (tico variable)
    (tico compilation))

  (data (layment layout compilation))

  (define-rule-syntax (test-layment $name)
    (make-layment
      (simple-layout)
      (test-compilation $name)))

  (define-rule-syntax (test-parameter-layment $name)
    (make-layment
      (simple-layout)
      (test-parameter-compilation $name)))

  (define (empty-layment)
    (layment (empty-layout) #f))

  (define-case-syntax (make-layment $layout $body)
    (lets
      ($var (generate-temporary))
      #`(lets
        (#,$var $layout)
        (layment #,$var
          (and (layout-not-empty? #,$var) $body)))))

  (define (literal->layment $literal)
    (layment
      (literal->layout $literal)
      (literal->compilation $literal)))

  (define (variable-layment $layout $datum $index)
    (make-layment $layout
      (variable-compilation $datum $index)))

  (define (layout-datum->layment $layout $datum)
    (make-layment $layout (datum->compilation $datum)))

  (define (bindings-layout-datum->layment $binding-layments $layout $datum)
    (layment $layout
      (bindings-datum->compilation
        (filter-opts (map layment-compilation $binding-layments))
        $datum)))

  (define (layment-datum $layment)
    (compilation-datum
      (layment-compilation $layment)))

  (define (layment-value $layment)
    (compilation-value
      (layment-compilation $layment)))

  (define (layment-not-empty? $layment)
    (layout-not-empty? (layment-layout $layment)))

  (define (layments->compilations $layments)
    (map layment-compilation
      (filter layment-not-empty? $layments)))

  (define (layment-application $target $args)
    (lets
      ($layout
        (layout-application
          (layment-layout $target)
          (map layment-layout $args)))
      (make-layment $layout
        (compilation-application
          (layout-arity $layout)
          (layment-compilation $target)
          (layments->compilations $args)))))

  (define (layment-abstraction $param-layments $body-layments)
    (scope-layment-abstraction
      (empty-stack-layment)
      $param-layments
      $body-layments))

  (define (scope-layment-abstraction $scope $param-layments $body-layments)
    (make-layment
      (layout-abstraction
        (map layment-layout $param-layments)
        (map layment-layout $body-layments))
      (compilation-abstraction
        (layment-compilation $scope)
        (filter-opts (map layment-compilation $param-layments))
        (map layment-compilation $body-layments))))

  (define (parameter-layment $layout $datum)
    (make-layment $layout
      (parameter-compilation $datum)))

  (define (generate-parameter-layment $layout)
    (make-layment $layout
      (generate-parameter-compilation)))

  (define (layment-parameter $layment)
    (layment
      (layment-layout $layment)
      (compilation-parameter (layment-compilation $layment))))

  (define (layment-variable $layment $index)
    (make-layment
      (layment-layout $layment)
      (compilation-variable (layment-compilation $layment) $index)))

  (define (layment-args $layments)
    (make-layment
      (layout-args
        (map layment-layout $layments))
      (compilation-args
        (filter-opts
          (map layment-compilation $layments)))))

  (define (layment-struct $name $field-layments)
    (make-layment
      (layout-struct $name
        (map layment-layout $field-layments))
      (compilation-struct $name
        (filter-opts
          (map layment-compilation $field-layments)))))

  (define (layment-ref $layment $index)
    (lets
      ($layout (layment-layout $layment))
      ($layout-field (layout-ref $layout $index))
      (make-layment
        (layout-field-layout $layout-field)
        (compilation-ref
          (struct-layout-size $layout)
          (layment-compilation $layment)
          (layout-field-index-opt $layout-field)))))

  (define (empty-stack-layment)
    (layment
      (empty-stack-layout)
      (empty-stack-compilation)))

  (define (stack-layment-push $stack-layment $layment)
    (layment
      (stack-layout-push
        (layment-layout $stack-layment)
        (layment-layout $layment))
      (lets
        ($stack-compilation (layment-compilation $stack-layment))
        ($compilation (layment-compilation $layment))
        (if $compilation
          (stack-compilation-push $stack-compilation $compilation)
          $stack-compilation))))

  (define (stack-layment-ref $stack-layment $index)
    (lets
      ($struct-layout (layment-layout $stack-layment))
      ($layout-field (stack-layout-ref $struct-layout $index))
      (layment
        (layout-field-layout $layout-field)
        (opt-lets
          ($index (layout-field-index-opt $layout-field))
          (stack-compilation-ref
            (layment-compilation $stack-layment)
            (- (struct-layout-size $struct-layout) $index 1))))))
)
