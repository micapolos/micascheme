(library (tico layment)
  (export
    layment layment? layment-layout layment-compilation
    layment-not-empty?
    test-layment

    make-layment
    empty-layment
    literal->layment
    variable-layment
    layout-datum->layment
    layout-datum->constant-layment

    layment-datum
    layment-value

    generate-parameter-layment
    layment-application
    layment-parameter
    layment-variable
    layment-abstraction
    layment-struct
    layment-ref)
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
    (layment $layout (datum->compilation $datum)))

  (define (layout-datum->constant-layment $layout $datum)
    (layment $layout (datum->constant-compilation $datum)))

  (define (layment-datum $layment)
    (compilation-top-level-datum
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

  (define (layment-abstraction $param-layments $body-layment)
    (make-layment
      (layout-abstraction
        (map layment-layout $param-layments)
        (layment-layout $body-layment))
      (compilation-abstraction
        (filter-opts (map layment-compilation $param-layments))
        (layment-compilation $body-layment))))

  (define (generate-parameter-layment $layout)
    (make-layment $layout
      (generate-parameter-compilation)))

  (define (layment-parameter $layment)
    (layment
      (layment-layout $layment)
      (compilation-parameter (layment-compilation $layment))))

  (define (layment-variable $layment $index)
    (layment
      (layment-layout $layment)
      (compilation-variable (layment-compilation $layment) $index)))

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
)
