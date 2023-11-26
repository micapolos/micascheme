(library (tico typing)
  (export
    typing typing? typing-type typing-layment
    static-typing
    test-typing
    test-static-typing
    test-parameter-typing

    native->typing
    literal->typing
    variable-typing
    type->typing
    boolean-typing
    number-typing
    string-typing
    type-datum->typing
    bindings-type-datum->typing
    single-typing

    typing-datum
    typing-value

    parameter-typing
    generate-parameter-typing
    typing-application
    typing-args-application
    typing-parameter
    typing-variable
    typing-argument
    scope-typing-abstraction
    typing-abstraction
    let-typing
    typing-args
    typing-struct
    typing-ref
    typing-ref-index
    typing-prepare
    typings-do
    typing-get
    typing-as
    typing-promising
    typing-property
    typing-offering
    typing-being
    typing-access
    typing-assert
    typing-argument-access

    typing-not-empty?
    typing->type
    typing->type-typing

    typing-resolve
    typings-resolve
    typings-resolve-get

    make-list-typing
    make-struct-typing

    empty-stack-typing
    stack-typing-push
    stack-typing
    stack-typing-ref
    stack-typing-type-ref

    typing-line
    typings-script
    typing-string
    typings-string)
  (import
    (micascheme)
    (tico type)
    (tico layment)
    (tico layout)
    (tico datum)
    (writing)
    (leo reader)
    (leo writing-reader))

  (data (typing type layment))

  (define-syntax-rule (test-typing $name)
    (type-datum->typing
      (test-type $name)
      (test-datum $name)))

  (define-syntax-rule (test-static-typing $name)
    (type-datum->typing
      (static-test-type $name)
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

  (define (bindings-type-datum->typing $binding-typings $type $datum)
    (typing $type
      (bindings-layout-datum->layment
        (map typing-layment $binding-typings)
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

  (define (typing-args-application $scope $target $args)
    (typing
      (type-args-application
        (typing-type $target)
        (typing-type $args))
      (layment-args-application
        (typing-layment $scope)
        (typing-layment $target)
        (typing-layment $args))))

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

  (define (typing-argument $key-typing $value-typing)
    (typing
      (argument-type
        (typing->type $key-typing)
        (typing-type $value-typing))
      (typing-layment $value-typing)))

  (define (typing-abstraction $param-typings $body-typing)
    (scope-typing-abstraction
      (empty-stack-typing)
      $param-typings
      $body-typing))

  (define (scope-typing-abstraction $scope $param-typings $body-typing)
    (typing
      (type-abstraction
        (map typing-type $param-typings)
        (list (typing-type $body-typing)))
      (scope-layment-abstraction
        (typing-layment $scope)
        (map typing-layment $param-typings)
        (list (typing-layment $body-typing)))))

  (define (typing-property $param-type $body-typing)
    (lets
      ($body-type (typing-type $body-typing))
      ($type (property $param-type $body-type))
      (typing $type
        (layment-abstraction
          (list (type->layout $param-type))
          (list (typing-layment $body-typing))))))

  (define (typing-access $typing $arg)
    (typing
      (type-access
        (typing-type $typing)
        (typing-type $arg))
      (layment-application
        (typing-layment $typing)
        (list (typing-layment $arg)))))

  (define (typing-argument-access $typing $arg)
    (typing
      (type-argument-access
        (typing-type $typing)
        (typing-type $arg))
      (typing-layment $typing)))

  (define (typing-args $typings)
    (typing
      (args-type
        (map typing-type $typings))
      (layment-args
        (map typing-layment $typings))))

  (define (typing-struct $name $field-typings)
    (typing
      (struct $name
        (map typing-type $field-typings))
      (layment-struct $name
        (map typing-layment $field-typings))))

  (define (typing-ref $typing $pattern)
    (opt-lets
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
          ((equal? $type (struct 'a (list (struct 'primitive (list)))))
            (type->typing (unchecked-type)))
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
              (opt-lets
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
        (list (typing->type $result-typing)))))

  (define (typing-offering $typing $offering-typing)
    (lets
      ($param (typing->type $typing))
      ($body (typing->type $offering-typing))
      (type->typing (property $param $body))))

  (define (typing-being $key-typing $value-typing)
    (type->typing
      (argument-type
        (typing->type $key-typing)
        (typing->type $value-typing))))

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
        (list (list-of $type)))
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

  (define (typings-do $scope $parameter-typings $argument-typings $body-typing)
    (typing-application
      (scope-typing-abstraction $scope $parameter-typings $body-typing)
      $argument-typings))

  (define (empty-stack-typing)
    (typing (stack) (empty-stack-layment)))

  (define (stack-typing-push $stack-typing $typing)
    (typing
      (push
        (typing-type $stack-typing)
        (typing-type $typing))
      (stack-layment-push
        (typing-layment $stack-typing)
        (typing-layment $typing))))

  (define (stack-typing-ref $stack-typing $index)
    (typing
      (list-ref (typing-type $stack-typing) $index)
      (stack-layment-ref (typing-layment $stack-typing) $index)))

  (define (stack-typing . $typings)
    (fold-left stack-typing-push (empty-stack-typing) $typings))

  (define (stack-typing-type-ref $stack-typing $type)
    (opt-lets
      ($indexed-type
        (types-match
          (typing-type $stack-typing)
          $type))
      (stack-typing-ref
        $stack-typing
        (indexed-index $indexed-type))))

  (define (typings-script $typings)
    (map typing-line $typings))

  (define (typing-line $typing)
    (lets
      ($type (typing-type $typing))
      (cond
        ((type-matches? $type (boolean-type))
          `(boolean
            ,(if (typing-value $typing) 'true 'false)))
        ((type-matches? $type (number-type))
          (typing-value $typing))
        ((type-matches? $type (string-type))
          (typing-value $typing))
        ((type-matches? $type (char-type))
          (typing-value $typing))
        (else
          (switch $type
            ((value-type? $value-type)
              `(type ,(type-line (value-type-value $value-type))))
            ((unchecked-type? _)
              `(primitive ,(typing-value $typing)))
            ((arrow? $arrow)
              (type-line $arrow))
            ((struct? $struct)
              (lets
                ($name (struct-name $struct))
                (switch (struct-fields $struct)
                  ((null? _) $name)
                  ((else $fields)
                    `(
                      ,$name
                      ,@(map typing-line
                        (map
                          (partial typing-ref-index $typing)
                          (enumerate $fields))))))))
            ((else $other)
              (throw typing-line $typing)))))))

  (define (typing-string $typing)
    (writing-string
      (reader-end
        (reader-read
          (writing-reader)
          (typing-line $typing)))))

  (define (typings-string $typings)
    (writing-string
      (reader-end
        (reader-read-list
          (writing-reader)
          (typings-script $typings)))))
)
