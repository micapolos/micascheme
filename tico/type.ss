(library (tico type)
  (export
    any-type any-type?
    value-type value-type? value-type-value
    native-type native-type?
    boolean-type
    number-type
    string-type
    char-type
    struct-type struct-type? struct-type-name struct-type-fields
    lambda-type lambda-type? lambda-type-params lambda-type-result

    type-dynamic?
    types-arity
    type-matches? types-match?)
  (import (micascheme))

  (data (any-type))
  (data (value-type value))
  (data (native-type))
  (data (struct-type name fields))
  (data (lambda-type params result))

  (define (boolean-type)
    (struct-type 'boolean (list (native-type))))

  (define (number-type)
    (struct-type 'number (list (native-type))))

  (define (string-type)
    (struct-type 'string (list (native-type))))

  (define (char-type)
    (struct-type 'char (list (native-type))))

  (define (type-dynamic? $type)
    (switch $type
      ((value-type? _) #f)
      ((native-type? _) #t)
      ((struct-type? $struct-type)
        (exists type-dynamic? (struct-type-fields $struct-type)))
      ((lambda-type? $lambda-type) #t)
      ((else $other)
        (throw not-type $other))))

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
      ((native-type? _)
        (native-type? $type))
      ((struct-type? $struct-type)
        (and
          (struct-type? $type)
          (symbol=? 
            (struct-type-name $type)
            (struct-type-name $struct-type))
          (types-match?
            (struct-type-fields $type)
            (struct-type-fields $struct-type))))
      ((lambda-type? $lambda-type)
        (and
          (lambda-type? $type)
          (types-match?
            (lambda-type-params $type)
            (lambda-type-params $lambda-type))
          (type-matches?
            (lambda-type-result $lambda-type)
            (lambda-type-result $type))))
      ((else $other)
        (throw not-type $pattern))))

  (define (types-match? $types $patterns)
    (and
      (= (length $types) (length $patterns))
      (for-all type-matches? $types $patterns)))
)
