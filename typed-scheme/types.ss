(library (typed-scheme types)
  (export
    null-type-definition
    boolean-type-definition
    char-type-definition
    string-type-definition
    number-type-definition
    integer-type-definition
    fixnum-type-definition
    pair-type-definition
    list-type-definition

    null-type
    boolean-type
    char-type
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
    (type-definition #f (gensym) "any-null" 0))

  (define boolean-type-definition
    (type-definition #f (gensym) "any-boolean" 0))

  (define char-type-definition
    (type-definition #f (gensym) "any-char" 0))

  (define string-type-definition
    (type-definition #f (gensym) "any-string" 0))

  (define number-type-definition
    (type-definition #f (gensym) "any-number" 0))

  (define integer-type-definition
    (type-definition number-type-definition (gensym) "any-integer" 0))

  (define fixnum-type-definition
    (type-definition integer-type-definition (gensym) "any-fixnum" 0))

  (define pair-type-definition
    (type-definition #f (gensym) "any-pair" 2))

  (define list-type-definition
    (type-definition #f (gensym) "any-list" 1))

  (define null-type
    (defined-type #f null-type-definition (immutable-vector)))

  (define boolean-type
    (defined-type #f boolean-type-definition (immutable-vector)))

  (define char-type
    (defined-type #f char-type-definition (immutable-vector)))

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
