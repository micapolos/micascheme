(library (tico typing)
  (export
    typing typing? typing-type typing-layment

    literal->typing
    type-datum->typing

    typing-datum
    typing-value

    typing-application
    typing-abstraction)
  (import
    (micascheme)
    (tico type)
    (tico layment)
    (tico layout))

  (data (typing type layment))

  (define (literal->typing $literal)
    (typing
      (literal->type $literal)
      (literal->layment $literal)))

  (define (type-datum->typing $type $datum)
    (typing $type
      (layout-datum->layment
        (type->layout $type)
        $datum)))

  (define (typing-datum $typing)
    (layment-datum
      (typing-layment $typing)))

  (define (typing-value $typing)
    (layment-value
      (typing-layment $typing)))

  (define (typing-application $target $args)
    (typing
      (type-application
        (typing-type $target)
        (map typing-type $args))
      (layment-application
        (typing-layment $target)
        (map typing-layment $args))))

  (define (typing-abstraction $param-types $body-typing)
    (typing
      (arrow $param-types (typing-type $body-typing))
      (layment-abstraction
        (map type->layout $param-types)
        (typing-layment $body-typing))))
)
