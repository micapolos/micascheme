(library (tico type)
  (export
    any-type any-type?
    value-type value-type? value-type-value
    unchecked-type unchecked-type?
    native-type native-type? native-type-value
    type-type type-type?
    args-type args-type? args-type-items
    list-of list-of? list-of-item-type
    boolean-type
    number-type
    string-type
    char-type
    symbol-type
    literal->type
    struct struct? struct-name struct-fields
    arrow arrow? arrow-params arrow-results
    property property? property-param property-body
    constant-type constant-type? constant-type-key constant-type-value
    abstraction abstraction? abstraction-arity abstraction-body
    recursion recursion? recursion-items
    var var? var-index
    type-value

    maybe-args-type
    type-flatten
    types-flatten

    test-type
    static-test-type

    failable-type-application
    type-application-opt
    type-application
    type-abstraction

    type-access
    type-access-opt

    type-constant-access-opt
    type-constant-access

    type-struct
    type-ref
    type-ref-index
    type-matches? types-match?
    types-match

    make-list-of
    make-struct-type

    type-line
    types-lines)
  (import
    (micascheme)
    (tico path))

  (data (any-type))
  (data (value-type value))
  (data (type-type))
  (data (unchecked-type))
  (data (native-type value))
  (data (args-type items))
  (data (list-of item-type))
  (data (struct name fields))
  (data (arrow params results))
  (data (property param body))
  (data (constant-type key value))
  (data (abstraction arity body))
  (data (recursion items))
  (data (var index))

  (define-rule-syntax (static-test-type $name)
    (struct (quote $name) (list)))

  (define-rule-syntax (test-type $name)
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
    (switch? $target
      ((arrow? $arrow)
        (and
          (types-match?
            (types-flatten $args)
            (arrow-params $arrow))
          (maybe-args-type
            (arrow-results $arrow))))))

  (define (failable-type-application $target $args)
    (switch $target
      ((arrow? $arrow)
        (cond
          ((types-match? (types-flatten $args) (arrow-params $arrow))
            (maybe-args-type (arrow-results $arrow)))
          (else
            (failure
              `(invalid
                (arguments
                  (expected ,@(arrow-params $arrow))
                  (actual ,@$args)))))))
      ((else $other)
        (failure `(not (arrow? ,$other))))))

  (define (type-application $target $args)
    (or-throw
      (type-application-opt $target $args)))

  (define (type-abstraction $param-types $body-types)
    (arrow $param-types $body-types))

  (define (type-access-opt $target $arg)
    (switch $target
      ((property? $property)
        (and
          (type-matches? $arg (property-param $property))
          (property-body $property)))))

  (define (type-access $target $arg)
    (or-throw
      (type-access-opt $target $arg)))

  (define (type-constant-access-opt $target $arg)
    (switch $target
      ((constant-type? $constant-type)
        (and
          (type-matches? $arg (constant-type-key $constant-type))
          (constant-type-value $constant-type)))))

  (define (type-constant-access $target $arg)
    (or-throw
      (type-constant-access-opt $target $arg)))

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
      ((native-type? $native-type)
        (and
          (native-type? $type)
          (equal?
            (native-type-value $type)
            (native-type-value $native-type))))
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
          (types-match?
            (arrow-results $type)
            (arrow-results $arrow))))
      ((property? $property)
        (and
          (property? $type)
          (type-matches?
            (property-param $property)
            (property-param $type))
          (type-matches?
            (property-body $type)
            (property-body $property))))
      ((constant-type? $constant-type)
        (and
          (constant-type? $type)
          (type-matches?
            (constant-type-key $constant-type)
            (constant-type-key $type))
          (type-matches?
            (constant-type-value $type)
            (constant-type-value $constant-type))))
      ((else $other)
        (throw not-type $pattern))))

  (define (types-match? $types $patterns)
    (and
      (= (length $types) (length $patterns))
      (for-all type-matches? $types $patterns)))

  (define (types-match-from $types $pattern $index)
    (switch $types
      ((null? _) #f)
      ((pair? (pair $type $types))
        (or
          (and (type-matches? $type $pattern) (indexed $type $index))
          (types-match-from $types $pattern (add1 $index))))))

  (define (types-match $types $pattern)
    (types-match-from $types $pattern 0))

  (define (type-struct $name $items)
    (struct $name (types-flatten $items)))

  (define (type-ref $type $pattern)
    (indexed-find
      (lambda ($index $type)
        (and
          (type-matches? $type $pattern)
          (indexed $type $index)))
      (struct-fields $type)))

  (define (type-ref-index $type $index)
    (list-ref (struct-fields $type) $index))

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
      (list (list-of $item-type))))

  (define (make-struct-type)
    (arrow
      (list (symbol-type) (list-of (type-type)))
      (list (type-type))))

  (define (types-lines $types)
    (map type-line $types))

  (define (type-line $type)
    (cond
      ((equal? $type (boolean-type)) 'boolean)
      ((equal? $type (number-type)) 'number)
      ((equal? $type (string-type)) 'string)
      ((equal? $type (char-type)) 'char)
      ((equal? $type (symbol-type)) 'symbol)
      (else 
        (switch $type
          ((any-type? _) 
            'any)
          ((value-type? $value-type)
            `(value ,(value-type-value $value-type)))
          ((type-type? _) 
            `type)
          ((unchecked-type? _)
            `primitive)
          ((native-type? $native-type)
            `(native ,(native-type-value $native-type)))
          ((struct? $struct)
            (switch (struct-fields $struct)
              ((null? _) 
                (struct-name $struct))
              ((else $fields)
                `(
                  ,(struct-name $struct)
                  ,@(types-lines $fields)))))
          ((arrow? $arrow)
            `(arrow
              ,@(map type-line (arrow-params $arrow))
              (promising ,@(types-lines (arrow-results $arrow)))))
          ((property? $property)
            `(property
              ,(type-line (property-param $property))
              (offering ,(type-line (property-body $property)))))
          ((else $other) $other)))))

  (define (maybe-args-type $types)
    (case (length $types)
      ((1) (car $types))
      (else (args-type $types))))

  (define (type-flatten $type)
    (switch $type
      ((args-type? $args-type)
        (args-type-items $args-type))
      ((else $other)
        (list $other))))

  (define (types-flatten $types)
    (apply append (map type-flatten $types)))
)
