(library (tico type)
  (export
    any-type any-type?
    value-type value-type? value-type-value
    native-type native-type?
    type-type type-type?
    list-of list-of? list-of-item-type
    boolean-type
    number-type
    string-type
    char-type
    literal->type
    struct struct? struct-name struct-fields
    arrow arrow? arrow-params arrow-result
    property property? property-params property-body
    abstraction abstraction? abstraction-arity abstraction-body
    recursion recursion? recursion-items
    var var? var-index
    type-value

    type-application-opt
    type-application
    type-abstraction

    type-access
    type-access-opt

    type-dynamic?
    types-arity
    type-ref
    type-matches? types-match?
    indexed-type-matching

    make-list-of
    make-struct-type)
  (import (micascheme))

  (data (any-type))
  (data (value-type value))
  (data (type-type))
  (data (native-type))
  (data (list-of item-type))
  (data (struct name fields))
  (data (arrow params result))
  (data (property params body))
  (data (abstraction arity body))
  (data (recursion items))
  (data (var index))

  (define (boolean-type)
    (struct 'boolean (list (native-type))))

  (define (number-type)
    (struct 'number (list (native-type))))

  (define (string-type)
    (struct 'string (list (native-type))))

  (define (char-type)
    (struct 'char (list (native-type))))

  (define (symbol-type)
    (struct 'symbol (list (native-type))))

  (define (literal->type $literal)
    (switch $literal
      ((boolean? _) (boolean-type))
      ((number? _) (number-type))
      ((string? _) (string-type))
      ((char? _) (char-type))
      ((symbol? _) (symbol-type))
      ((else $other) (throw literal->type $literal))))

  (define (type-dynamic? $type)
    (switch $type
      ((value-type? _) #f)
      ((native-type? _) #t)
      ((type-type? _) #t)
      ((struct? $struct)
        (exists type-dynamic? (struct-fields $struct)))
      ((arrow? $arrow)
        (type-dynamic? (arrow-result $arrow)))
      ((property? $property)
        (type-dynamic? (property-body $property)))
      ((else $other)
        (throw not-type $other))))

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

  (define (type-access-opt $target $args)
    (switch $target
      ((property? $property)
        (and
          (types-match? $args (property-params $property))
          (property-body $property)))))

  (define (type-access $target $args)
    (or-throw
      (type-access-opt $target $args)))

  (define (types-arity $types)
    (length (filter type-dynamic? $types)))

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
      ((native-type? _)
        (native-type? $type))
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
          (types-match?
            (property-params $property)
            (property-params $type))
          (type-matches?
            (property-body $type)
            (property-body $property))))
      ((else $other)
        (throw not-type $pattern))))

  (define (types-match? $types $patterns)
    (and
      (= (length $types) (length $patterns))
      (for-all type-matches? $types $patterns)))

  (define (indexed-type-matching-from $types $pattern $from)
    (switch $types
      ((null? _) #f)
      ((pair? $pair)
        (unpair $pair $type $types
          (lets
            ($dynamic? (type-dynamic? $type))
            (cond
              ((type-matches? $type $pattern)
                (indexed $type (and $dynamic? $from)))
              (else
                (indexed-type-matching-from $types $pattern
                  (if $dynamic? (+ $from 1) $from)))))))))

  (define (indexed-type-matching $types $pattern)
    (indexed-type-matching-from $types $pattern 0))

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
