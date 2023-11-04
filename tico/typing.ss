(library (tico typing)
  (export
    typing typing? typing-type typing-layment

    literal->typing
    type-datum->typing

    typing-application)
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

  (define (typing-application $target $args)
    (typing
      (type-application
        (typing-type $target)
        (map typing-type $args))
      (layment-application
        (typing-layment $target)
        (map typing-layment $args))))
)
