(library (tico type)
  (export
    value-type value-type? value-type-value
    type-type type-type?
    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?
    struct-type struct-type? struct-type-name struct-type-fields
    lambda-type lambda-type? lambda-type-params lambda-type-result

    type-dynamic?)
  (import (micascheme))

  (data (value-type value))
  (data (type-type))
  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (char-type))
  (data (struct-type name fields))
  (data (lambda-type params result))

  (define (type-dynamic? $type)
    (switch $type
      ((value-type? _) #f)
      ((boolean-type? _) #t)
      ((number-type? _) #t)
      ((string-type? _) #t)
      ((struct-type? $struct-type)
        (exists type-dynamic? (struct-type-fields $struct-type)))
      ((lambda-type? $lambda-type)
        (type-dynamic? (lambda-type-result $lambda-type)))
      ((else $other)
        (throw not-type $other))))
)
