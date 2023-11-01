(library (tico type)
  (export
    value-type value-type? value-type-value
    native-type native-type?
    boolean-type
    number-type
    string-type
    char-type
    struct-type struct-type? struct-type-name struct-type-fields
    lambda-type lambda-type? lambda-type-params lambda-type-result

    type-dynamic?
    type-matches?)
  (import (micascheme))

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

  (define (type-matches? $type $pattern)
    (equal? $type $pattern))
)
