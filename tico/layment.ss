(library (tico layment)
  (export
    layment layment? layment-layout layment-compilation
    layment-not-empty?

    make-layment
    empty-layment
    literal->layment
    layout-datum->layment

    layment-datum
    layment-value

    generate-parameter-layment
    layment-application
    layment-parameter
    layment-abstraction
    layment-struct)
  (import
    (micascheme)
    (tico datum)
    (tico layout)
    (tico constant)
    (tico variable)
    (tico compilation))

  (data (layment layout compilation))

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

  (define (layout-datum->layment $layout $datum)
    (layment $layout (datum->compilation $datum)))

  (define (layment-datum $layment)
    (compilation-top-level-datum
      (layment-compilation $layment)))

  (define (layment-value $layment)
    (compilation-value
      (layment-compilation $layment)))

  (define (layment-not-empty? $layment)
    (layout-not-empty? (layment-layout $layment)))

  (define (layments->compilations $layments)
    (map layment-compilation (filter layment-not-empty? $layments)))

  (define (layment-application $target $args)
    (make-layment
      (layout-application
        (layment-layout $target)
        (map layment-layout $args))
      (compilation-application
        (layment-compilation $target)
        (layments->compilations $args))))

  (define (generate-parameter-layment $layout)
    (layment $layout
      (generate-parameter-compilation)))

  (define (layment-parameter $layment)
    (layment
      (layment-layout $layment)
      (compilation-parameter (layment-compilation $layment))))

  (define (layment-abstraction $param-layouts $body-compilation)
    TODO)

  (define (layment-struct $name $field-layments)
    (make-layment
      (layout-struct $name
        (map layment-layout $field-layments))
      (compilation-struct $name
        (map layment-compilation $field-layments))))
)
