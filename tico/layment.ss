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

    layment-application
    layment-abstraction)
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
      (simple-layout)
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

  (define (layment-abstraction) TODO)
)
