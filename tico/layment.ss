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
    layment-args-application
    layment-parameter
    layment-variable
    scope-layment-abstraction
    layment-abstraction
    layment-args
    layment-struct
    layment-ref

    empty-layment-scope
    layment-scope-push
    layment-scope-ref)
  (import
    (micascheme)
    (tico datum)
    (tico layout)
    (tico constant)
    (tico variable)
    (tico compilation))

  (data (layment layout compilation))

  (define-syntax-rule (test-layment $name)
    (make-layment
      (simple-layout)
      (test-compilation $name)))

  (define-syntax-rule (test-parameter-layment $name)
    (make-layment
      (simple-layout)
      (test-parameter-compilation $name)))

  (define (empty-layment)
    (layment (empty-layout) #f))

  (define-syntax-case (make-layment $layout $body)
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
    (make-layment
      (layout-application
        (layment-layout $target)
        (map layment-layout $args))
      (compilation-application
        (layment-compilation $target)
        (layments->compilations $args))))

  (define (layment-args-application $scope $target $args)
    (make-layment
      (layout-args-application
        (layment-layout $target)
        (layment-layout $args))
      (compilation-args-application
        (layment-compilation $scope)
        (layment-compilation $target)
        (layment-compilation $args))))

  (define (layment-abstraction $param-layments $body-layment)
    (scope-layment-abstraction
      (empty-layment-scope) $param-layments $body-layment))

  (define (scope-layment-abstraction $scope $param-layments $body-layment)
    (make-layment
      (layout-abstraction
        (map layment-layout $param-layments)
        (layment-layout $body-layment))
      (compilation-abstraction
        (layment-compilation $scope)
        (filter-opts (map layment-compilation $param-layments))
        (layment-compilation $body-layment))))

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

  (define (empty-layment-scope)
    (layment
      (empty-layout-scope)
      (empty-compilation-scope)))

  (define (layment-scope-push $layment-scope $layment)
    (layment
      (layout-scope-push
        (layment-layout $layment-scope)
        (layment-layout $layment))
      (lets
        ($compilation-scope (layment-compilation $layment-scope))
        ($compilation (layment-compilation $layment))
        (if $compilation
          (compilation-scope-push $compilation-scope $compilation)
          $compilation-scope))))

  (define (layment-scope-ref $layment-scope $index)
    (lets
      ($struct-layout (layment-layout $layment-scope))
      ($layout-field (layout-scope-ref $struct-layout $index))
      (layment
        (layout-field-layout $layout-field)
        (opt-lets
          ($index (layout-field-index-opt $layout-field))
          (compilation-scope-ref
            (layment-compilation $layment-scope)
            (- (struct-layout-size $struct-layout) $index 1))))))
)
