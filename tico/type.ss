(library (tico type)
  (export
    type-type type-type?
    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?
    function-type function-type? function-type-params function-type-body
    struct-type struct-type? struct-type-name struct-type-items
    enum-type enum-type? enum-type-name enum-type-items

    type-matches?)
  (import (micascheme))

  (data (type-type))
  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (function-type params body))
  (data (struct-type name items))
  (data (enum-type name items))

  (define (type-matches? $type $pattern)
    (equal? $type $pattern))
)
