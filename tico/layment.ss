(library (tico layment)
  (export
    layment layment? layment-layout layment-compilation
    layment-not-empty?

    make-layment
    empty-layment
    literal->layment
    layout-datum->layment

    layment-application)
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

  (define-syntax-rule (make-layment ($var $layout) $body)
    (lets
      ($var $layout)
      (layment $var
        (and (layout-not-empty? $var) $body))))

  (define (literal->layment $literal)
    (layment
      (simple-layout)
      (literal->compilation $literal)))

  (define (layout-datum->layment $layout $datum)
    (layment $layout (datum->compilation $datum)))

  (define (layment-not-empty? $layment)
    (layout-not-empty? (layment-layout $layment)))

  (define (layments->compilations $layments)
    (map layment-compilation (filter layment-not-empty? $layments)))

  (define (layment-application $target $args)
    (lets
      ($lambda-layout (ensure lambda-layout? (layment-layout $target)))
      ($arg-layouts (map layment-layout $args))
      (make-layment
        ($layout (lambda-layout-body $lambda-layout))
        (compilation-application
          (layment-compilation $target)
          (layments->compilations $args)))))
)
