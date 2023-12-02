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

  (define-syntax-rule (test-layment $name)
    (make-layment
      (simple-layout)
      (test-compilation $name)))

  (define-syntax-rule (test-parameter-layment $name)
    (make-layment
      (simple-layout)
      (test-parameter-compilation $name)))

  (function (empty-layment)
    (layment (empty-layout) #f))

  (define-syntax-case (make-layment $layout $body)
    (lets
      ($var (generate-temporary))
      #`(lets
        (#,$var $layout)
        (layment #,$var
          (and (layout-not-empty? #,$var) $body)))))

  (function (literal->layment $literal)
    (layment
      (literal->layout $literal)
      (literal->compilation $literal)))

  (function (variable-layment $layout $datum $index)
    (make-layment $layout
      (variable-compilation $datum $index)))

  (function (layout-datum->layment $layout $datum)
    (make-layment $layout (datum->compilation $datum)))

  (function (bindings-layout-datum->layment $binding-layments $layout $datum)
    (layment $layout
      (bindings-datum->compilation
        (filter-opts (map layment-compilation $binding-layments))
        $datum)))

  (function (layment-datum $layment)
    (compilation-datum
      (layment-compilation $layment)))

  (function (layment-value $layment)
    (compilation-value
      (layment-compilation $layment)))

  (function (layment-not-empty? $layment)
    (layout-not-empty? (layment-layout $layment)))

  (function (layments->compilations $layments)
    (map layment-compilation
      (filter layment-not-empty? $layments)))

  (function (layment-application $target $args)
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

  (function (layment-abstraction $param-layments $body-layments)
    (scope-layment-abstraction
      (empty-stack-layment)
      $param-layments
      $body-layments))

  (function (scope-layment-abstraction $scope $param-layments $body-layments)
    (make-layment
      (layout-abstraction
        (map layment-layout $param-layments)
        (map layment-layout $body-layments))
      (compilation-abstraction
        (layment-compilation $scope)
        (filter-opts (map layment-compilation $param-layments))
        (map layment-compilation $body-layments))))

  (function (parameter-layment $layout $datum)
    (make-layment $layout
      (parameter-compilation $datum)))

  (function (generate-parameter-layment $layout)
    (make-layment $layout
      (generate-parameter-compilation)))

  (function (layment-parameter $layment)
    (layment
      (layment-layout $layment)
      (compilation-parameter (layment-compilation $layment))))

  (function (layment-variable $layment $index)
    (make-layment
      (layment-layout $layment)
      (compilation-variable (layment-compilation $layment) $index)))

  (function (layment-args $layments)
    (make-layment
      (layout-args
        (map layment-layout $layments))
      (compilation-args
        (filter-opts
          (map layment-compilation $layments)))))

  (function (layment-struct $name $field-layments)
    (make-layment
      (layout-struct $name
        (map layment-layout $field-layments))
      (compilation-struct $name
        (filter-opts
          (map layment-compilation $field-layments)))))

  (function (layment-ref $layment $index)
    (lets
      ($layout (layment-layout $layment))
      ($layout-field (layout-ref $layout $index))
      (make-layment
        (layout-field-layout $layout-field)
        (compilation-ref
          (struct-layout-size $layout)
          (layment-compilation $layment)
          (layout-field-index-opt $layout-field)))))

  (function (empty-stack-layment)
    (layment
      (empty-stack-layout)
      (empty-stack-compilation)))

  (function (stack-layment-push $stack-layment $layment)
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

  (function (stack-layment-ref $stack-layment $index)
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
