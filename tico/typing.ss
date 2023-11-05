(library (tico typing)
  (export
    typing typing? typing-type typing-layment
    static-typing

    native->typing
    literal->typing
    type-datum->typing

    typing-datum
    typing-value

    typing-application
    typing-abstraction
    typing-struct
    typing-native

    typing-inline)
  (import
    (micascheme)
    (tico type)
    (tico layment)
    (tico layout)
    (tico datum))

  (data (typing type layment))

  (define (static-typing $type)
    (typing $type
      (make-layment
        (type->layout $type)
        (throw not-static))))

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

  (define (typing-struct $name $field-typings)
    (typing
      (struct $name
        (map typing-type $field-typings))
      (layment-struct $name
        (map typing-layment $field-typings))))

  (define (typing-native $typing)
    (native->typing
      (string->read-datum
        (ensure string?
          (typing-value $typing)))))

  (define (native->typing $datum)
    (type-datum->typing (native-type) $datum))

  (define (typing-inline $typing)
    (type-datum->typing
      (typing-type $typing)
      (value->datum (typing-value $typing))))
)
