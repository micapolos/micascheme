(library (tico typing)
  (export
    typing typing? typing-type typing-layment
    static-typing

    native->typing
    literal->typing
    type->typing
    boolean-typing
    number-typing
    string-typing
    type-datum->typing
    single-typing

    typing-datum
    typing-value

    generate-parameter-typing
    typing-application
    typing-parameter
    typing-variable
    typing-abstraction
    typing-struct
    typing-ref
    typing-native
    typing-inline
    typings-get
    typing-as
    typings-giving

    typing-not-empty?

    typing-resolve)
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

  (define (type->typing $type)
    (static-typing (value-type $type)))

  (define (boolean-typing)
    (type->typing (boolean-type)))

  (define (number-typing)
    (type->typing (number-type)))

  (define (string-typing)
    (type->typing (string-type)))

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

  (define (generate-parameter-typing $type)
    (typing $type
      (generate-parameter-layment (type->layout $type))))

  (define (typing-parameter $typing)
    (typing
      (typing-type $typing)
      (layment-parameter (typing-layment $typing))))

  (define (typing-variable $typing $index)
    (typing
      (typing-type $typing)
      (layment-variable (typing-layment $typing) $index)))

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

  (define (typing-ref $typing $pattern)
    (lets
      ($indexed-type
        (ensure indexed?
          (type-ref (typing-type $typing) $pattern)))
      (typing
        (indexed-value $indexed-type)
        (layment-ref
          (typing-layment $typing)
          (indexed-index $indexed-type)))))

  (define (typings-get $typings $selector-typings)
    (fold-left
      typing-ref
      (single-typing $typings)
      (reverse (map typing->type $selector-typings))))

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

  (define (typing-not-empty? $typing)
    (layment-not-empty? (typing-layment $typing)))

  (define (typing-resolve $typing)
    (lets
      ($type (typing-type $typing))
        (cond
          ((equal? $type (struct 'boolean (list)))
            (boolean-typing))
          ((equal? $type (struct 'number (list)))
            (number-typing))
          ((equal? $type (struct 'string (list)))
            (string-typing))
          (else $typing))))

  (define (typing->type $typing)
    (type-value (typing-type $typing)))

  (define (typing-as $typing $type-typing)
    (switch (typing-type $typing)
      ((native-type? _)
        (type-compilation->layment
          (typing->type $type-typing)
          (layment-compilation (typing-layment $typing))))
      ((else $other)
        TODO)))

  (define (type-compilation->layment $type $compilation)
    (typing $type
      (make-layment
        (type->layout $type)
        $compilation)))

  (define (typings-giving $param-typings $result-typing)
    (type->typing
      (arrow
        (map typing->type $param-typings)
        (typing->type $result-typing))))

  (define (single-typing $typings)
    (car (ensure single? $typings)))
)
