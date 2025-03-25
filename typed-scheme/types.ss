(library (typed-scheme types)
  (export
    object-type-definition
    null-type-definition
    boolean-type-definition
    string-type-definition
    number-type-definition
    integer-type-definition
    fixnum-type-definition
    pair-type-definition
    list-type-definition

    object-type
    boolean-type
    string-type
    number-type
    integer-type
    fixnum-type
    pair-type
    list-type)
  (import
    (micascheme)
    (typed-scheme type))

  (data object-type-definition
    (type-definition #f (gensym) "object" 0))

  (data null-type-definition
    (type-definition object-type-definition (gensym) "null" 0))

  (data boolean-type-definition
    (type-definition object-type-definition (gensym) "boolean" 0))

  (data string-type-definition
    (type-definition object-type-definition (gensym) "string" 0))

  (data number-type-definition
    (type-definition object-type-definition (gensym) "number" 0))

  (data integer-type-definition
    (type-definition number-type-definition (gensym) "integer" 0))

  (data fixnum-type-definition
    (type-definition integer-type-definition (gensym) "fixnum" 0))

  (data pair-type-definition
    (type-definition #f (gensym) "pair" 2))

  (data list-type-definition
    (type-definition #f (gensym) "list" 1))

  (define object-type
    (defined-type #f object-type-definition (immutable-vector)))

  (define null-type
    (defined-type object-type null-type-definition (immutable-vector)))

  (define boolean-type
    (defined-type object-type boolean-type-definition (immutable-vector)))

  (define string-type
    (defined-type object-type string-type-definition (immutable-vector)))

  (define number-type
    (defined-type object-type number-type-definition (immutable-vector)))

  (define integer-type
    (defined-type number-type integer-type-definition (immutable-vector)))

  (define fixnum-type
    (defined-type integer-type fixnum-type-definition (immutable-vector)))

  (define (pair-type $first $second)
    (defined-type object-type pair-type-definition (immutable-vector $first $second)))

  (define (list-type $item)
    (recursive-type
      (union-type
        (list
          null-type
          (pair-type $item (variable-type 0))))))
)
