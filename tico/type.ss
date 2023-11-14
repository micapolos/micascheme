(library (tico type)
  (export
    any-type any-type?
    value-type value-type? value-type-value
    unchecked-type unchecked-type?
    type-type type-type?
    list-of list-of? list-of-item-type
    boolean-type
    number-type
    string-type
    char-type
    literal->type
    struct struct? struct-name struct-fields
    arrow arrow? arrow-params arrow-result
    property property? property-param property-body
    abstraction abstraction? abstraction-arity abstraction-body
    recursion recursion? recursion-items
    var var? var-index
    type-value

    test-type
    static-test-type

    type-application-opt
    type-application
    type-abstraction

    type-access
    type-access-opt

    type-ref
    type-matches? types-match?

    make-list-of
    make-struct-type)
  (import
    (micascheme)
    (tico path))

  (data (any-type))
  (data (value-type value))
  (data (type-type))
  (data (unchecked-type))
  (data (list-of item-type))
  (data (struct name fields))
  (data (arrow params result))
  (data (property param body))
  (data (abstraction arity body))
  (data (recursion items))
  (data (var index))

  (define-syntax-rule (static-test-type $name)
    (struct (quote $name) (list)))

  (define-syntax-rule (test-type $name)
    (struct (quote $name) (list (unchecked-type))))

  (define (boolean-type)
    (struct 'boolean (list (unchecked-type))))

  (define (number-type)
    (struct 'number (list (unchecked-type))))

  (define (string-type)
    (struct 'string (list (unchecked-type))))

  (define (char-type)
    (struct 'char (list (unchecked-type))))

  (define (symbol-type)
    (struct 'symbol (list (unchecked-type))))

  (define (literal->type $literal)
    (switch $literal
      ((boolean? _) (boolean-type))
      ((number? _) (number-type))
      ((string? _) (string-type))
      ((char? _) (char-type))
      ((symbol? _) (symbol-type))
      ((else $other) (throw literal->type $literal))))

  (define (type-application-opt $target $args)
    (switch-opt $target
      ((arrow? $arrow)
        (and
          (types-match? $args (arrow-params $arrow))
          (arrow-result $arrow)))))

  (define (type-application $target $args)
    (or-throw
      (type-application-opt $target $args)))

  (define (type-abstraction $param-types $body-type)
    (arrow $param-types $body-type))

  (define (type-access-opt $target $arg)
    (switch $target
      ((property? $property)
        (and
          (type-matches? $arg (property-param $property))
          (property-body $property)))))

  (define (type-access $target $arg)
    (or-throw
      (type-access-opt $target $arg)))

  (define (type-matches? $type $pattern)
    (switch $pattern
      ((any-type? _) 
        #t)
      ((value-type? $value-type)
        (and
          (value-type? $type)
          (equal? 
            (value-type-value $type)
            (value-type-value $value-type))))
      ((type-type? _)
        (type-type? $type))
      ((unchecked-type? _)
        (unchecked-type? $type))
      ((struct? $struct)
        (and
          (struct? $type)
          (symbol=? 
            (struct-name $type)
            (struct-name $struct))
          (or
            (null? (struct-fields $struct))
            (types-match?
              (struct-fields $type)
              (struct-fields $struct)))))
      ((arrow? $arrow)
        (and
          (arrow? $type)
          (types-match?
            (arrow-params $arrow)
            (arrow-params $type))
          (type-matches?
            (arrow-result $type)
            (arrow-result $arrow))))
      ((property? $property)
        (and
          (property? $type)
          (type-matches?
            (property-param $property)
            (property-param $type))
          (type-matches?
            (property-body $type)
            (property-body $property))))
      ((else $other)
        (throw not-type $pattern))))

  (define (types-match? $types $patterns)
    (and
      (= (length $types) (length $patterns))
      (for-all type-matches? $types $patterns)))

  (define (type-ref $type $pattern)
    (indexed-find
      (lambda ($index $type)
        (and
          (type-matches? $type $pattern)
          (indexed $type $index)))
      (struct-fields $type)))

  (define (type-value $type)
    (switch $type
      ((value-type? $value-type)
        (value-type-value $value-type))
      ((struct? $struct)
        (struct
          (struct-name $struct)
          (map type-value (struct-fields $struct))))
      ((else $other)
        (throw type-value $other))))

  (define (make-list-of $arity $item-type)
    (arrow
      (make-list $arity $item-type)
      (list-of $item-type)))

  (define (make-struct-type)
    (arrow
      (list (symbol-type) (list-of (type-type)))
      (type-type)))
)
