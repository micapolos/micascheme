(library (tico typing)
  (export
    typing typing? typing-type typing-layment

    literal->typing
    type-datum->typing)
  (import
    (micascheme)
    (tico type)
    (tico layment)
    (tico layout))

  (data (typing type layment))

  (define (literal->typing $literal)
    (typing
      (literal-type $literal)
      (literal->layment $literal)))

  (define (type-datum->typing $type $datum)
    (typing $type
      (layout-datum->layment
        (type->layout $type)
        $datum)))
)
