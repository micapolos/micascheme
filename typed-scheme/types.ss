(library (typed-scheme types)
  (export
    null-type-definition
    boolean-type-definition
    string-type-definition
    number-type-definition
    integer-type-definition
    fixnum-type-definition
    pair-type-definition
    list-type-definition

    null-type
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

  (define null-type-definition
    (type-definition #f (gensym) "null" 0))

  (define boolean-type-definition
    (type-definition #f (gensym) "boolean" 0))

  (define string-type-definition
    (type-definition #f (gensym) "string" 0))

  (define number-type-definition
    (type-definition #f (gensym) "number" 0))

  (define integer-type-definition
    (type-definition number-type-definition (gensym) "integer" 0))

  (define fixnum-type-definition
    (type-definition integer-type-definition (gensym) "fixnum" 0))

  (define pair-type-definition
    (type-definition #f (gensym) "pair" 2))

  (define list-type-definition
    (type-definition #f (gensym) "list" 1))

  (define null-type
    (defined-type #f null-type-definition (immutable-vector)))

  (define boolean-type
    (defined-type #f boolean-type-definition (immutable-vector)))

  (define string-type
    (defined-type #f string-type-definition (immutable-vector)))

  (define number-type
    (defined-type #f number-type-definition (immutable-vector)))

  (define integer-type
    (defined-type number-type integer-type-definition (immutable-vector)))

  (define fixnum-type
    (defined-type integer-type fixnum-type-definition (immutable-vector)))

  (define (pair-type $first $second)
    (defined-type #f pair-type-definition (immutable-vector $first $second)))

  (define (list-type $item)
    (recursive-type
      (union-type
        (list
          null-type
          (pair-type $item (variable-type 0))))))
)
