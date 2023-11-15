(library (tico typing)
  (export
    typing typing? typing-type typing-layment
    static-typing
    test-typing
    test-parameter-typing

    native->typing
    literal->typing
    variable-typing
    type->typing
    boolean-typing
    number-typing
    string-typing
    type-datum->typing
    single-typing

    typing-datum
    typing-value

    parameter-typing
    generate-parameter-typing
    typing-application
    typing-parameter
    typing-variable
    typing-abstraction
    let-typing
    typing-struct
    typing-ref
    typing-ref-index
    typing-native
    typing-prepare
    typings-do
    typing-get
    typing-as
    typing-promising
    typing-property
    typing-offering
    typing-access
    typing-assert

    typing-not-empty?
    typing->type
    typing->type-typing

    typing-resolve
    typings-resolve
    typings-resolve-get

    make-list-typing
    make-struct-typing

    typing-line
    typings-lines)
  (import
    (micascheme)
    (tico type)
    (tico layment)
    (tico layout)
    (tico datum))

  (data (typing type layment))

  (define-syntax-rule (test-typing $name)
    (type-datum->typing
      (test-type $name)
      (test-datum $name)))

  (define-syntax-rule (test-parameter-typing $name)
    (typing
      (test-type $name)
      (test-parameter-layment $name)))

  (define (static-typing $type)
    (typing $type
      (make-layment
        (type->layout $type)
        (throw not-static))))

  (define (literal->typing $literal)
    (typing
      (literal->type $literal)
      (literal->layment $literal)))

  (define (variable-typing $type $datum $index)
    (typing $type
      (variable-layment (type->layout $type) $datum $index)))

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

  (define (parameter-typing $type $datum)
    (typing
      $type
      (parameter-layment
        (type->layout $type)
        $datum)))

  (define (generate-parameter-typing $type)
    (typing $type
      (generate-parameter-layment
        (type->layout $type))))

  (define (typing-parameter $typing)
    (typing
      (typing-type $typing)
      (layment-parameter (typing-layment $typing))))

  (define (typing-variable $typing $index)
    (typing
      (typing-type $typing)
      (layment-variable (typing-layment $typing) $index)))

  (define (typing-abstraction $param-typings $body-typing)
    (typing
      (type-abstraction
        (map typing-type $param-typings)
        (typing-type $body-typing))
      (layment-abstraction
        (map typing-layment $param-typings)
        (typing-layment $body-typing))))

  (define (typing-property $param-type $body-typing)
    (lets
      ($body-type (typing-type $body-typing))
      ($type (property $param-type $body-type))
      (typing $type
        (layment-abstraction
          (list (type->layout $param-type))
          (typing-layment $body-typing)))))

  (define (typing-access $typing $arg)
    (typing
      (type-access
        (typing-type $typing)
        (typing-type $arg))
      (layment-application
        (typing-layment $typing)
        (list (typing-layment $arg)))))

  (define (typing-struct $name $field-typings)
    (typing
      (struct $name
        (map typing-type $field-typings))
      (layment-struct $name
        (map typing-layment $field-typings))))

  (define (typing-ref $typing $pattern)
    (and-lets
      ($indexed-type (type-ref (typing-type $typing) $pattern))
      (typing
        (indexed-value $indexed-type)
        (layment-ref
          (typing-layment $typing)
          (indexed-index $indexed-type)))))

  (define (typing-ref-index $typing $index)
    (typing
      (type-ref-index (typing-type $typing) $index)
      (layment-ref
        (typing-layment $typing)
        $index)))

  (define (typing-get $typing $patterns)
    (fold-left typing-ref $typing $patterns))

  (define (typing-native $typing)
    (native->typing
      (string->read-datum
        (ensure string?
          (typing-value $typing)))))

  (define (native->typing $datum)
    (type-datum->typing (unchecked-type) $datum))

  (define (typing-prepare $typing)
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

  (define (typings-resolve $typings)
    (typings-resolve-get $typings))

  (define (typings-resolve-get $typings)
    (and
      (= (length $typings) 2)
      (lets
        ($target-typing (cadr $typings))
        ($selector-typing (car $typings))
        (switch-opt (typing-type $selector-typing)
          ((struct? $selector-struct)
            (and
              (symbol=? (struct-name $selector-struct) 'get)
              (and-lets
                ($selector-type (single (struct-fields $selector-struct)))
                (typing-ref $target-typing (type-value $selector-type)))))))))

  (define (typing->type $typing)
    (type-value (typing-type $typing)))

  (define (typing->type-typing $typing)
    (type-datum->typing 
      (type-type)
      (value->datum 
        (type-value 
          (typing-type $typing)))))

  (define (typing-as $typing $type-typing)
    (switch (typing-type $typing)
      ((unchecked-type? _)
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

  (define (typing-promising $param-typings $result-typing)
    (type->typing
      (arrow
        (map typing->type $param-typings)
        (typing->type $result-typing))))

  (define (typing-offering $typing $offering-typing)
    (lets
      ($param (typing->type $typing))
      ($body (typing->type $offering-typing))
      (type->typing (property $param $body))))

  (define (single-typing $typings)
    (car (ensure single? $typings)))

  (define (let-typing $typings $fn)
    (lets
      ($types (map typing-type $typings))
      ($parameter-typings (ordered-map generate-parameter-typing $types))
      ($variable-typings
        (map
          typing-variable
          $parameter-typings
          (reverse (indices (length $parameter-typings)))))
      (typing-application
        (typing-abstraction $parameter-typings ($fn $variable-typings))
        $typings)))

  (define (make-list-typing $arity $type)
    (type-datum->typing
      (arrow
        (make-list $arity $type)
        (list-of $type))
      'list))

  (define (make-struct-typing)
    (type-datum->typing
      (make-struct-type)
      'struct))

  (define (struct-typing $struct)
    (lets
      ($fields (struct-fields $struct))
      (let-typing
        (map type-typing $fields)
        (lambda ($field-typings)
          (typing-application
            (make-struct-typing)
            (list
              (literal->typing (struct-name $struct))
              (typing-application
                (make-list-typing (length $fields) (type-type))
                $field-typings)))))))

  (define (typing-assert $typing)
    (unless (type-matches? (typing-type $typing) (boolean-type))
      (throw not-boolean (typing-datum $typing)))
    (unless (typing-value $typing)
      (throw assertiong-failed (typing-datum $typing))))

  (define (type-typing $typing)
    (switch (typing-type $typing)
      ((struct? $struct) (struct-typing $struct))
      ((else $other) TODO)))

  (define (typings-do $parameter-typings $argument-typings $body-typing)
    (typing-application
      (typing-abstraction $parameter-typings $body-typing)
      $argument-typings))

  (define (typings-lines $typings)
    (map typing-line $typings))

  (define (typing-line $typing)
    (lets
      ($type (typing-type $typing))
      (cond
        ((equal? $type (boolean-type))
          (typing-value $typing))
        ((equal? $type (number-type))
          (typing-value $typing))
        ((equal? $type (string-type))
          (typing-value $typing))
        ((equal? $type (char-type))
          (typing-value $typing))
        ((equal? $type (symbol-type))
          (typing-value $typing))
        (else
          (switch $type
            ((struct? $struct)
              (switch (struct-fields $struct)
                ((null? _) (struct-name $struct))
                ((else $fields)
                  `(
                    ,(struct-name struct)
                    ,@(let-typing $typing
                      (lambda ($typing)
                        (map typing-line
                          (typing-value
                            (map
                              (partial typing-ref-index $typing)
                              (indices (length $fields)))))))))))
            ((else $type)
              `(native
                ,(format "~s"
                  (typing-datum $typing)))))))))
)
