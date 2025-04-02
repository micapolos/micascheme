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
    (type-definition #f (gensym) "any-null" (list)))

  (define boolean-type-definition
    (type-definition #f (gensym) "any-boolean" (list)))

  (define char-type-definition
    (type-definition #f (gensym) "any-char" (list)))

  (define string-type-definition
    (type-definition #f (gensym) "any-string" (list)))

  (define number-type-definition
    (type-definition #f (gensym) "any-number" (list)))

  (define integer-type-definition
    (type-definition number-type-definition (gensym) "any-integer" (list)))

  (define fixnum-type-definition
    (type-definition integer-type-definition (gensym) "any-fixnum" (list)))

  (define pair-type-definition
    (type-definition #f (gensym) "any-pair" (list out-variance out-variance)))

  (define list-type-definition
    (type-definition #f (gensym) "any-list" (list out-variance)))

  (define null-type
    (defined-type #f null-type-definition (list)))

  (define boolean-type
    (defined-type #f boolean-type-definition (list)))

  (define char-type
    (defined-type #f char-type-definition (list)))

  (define string-type
    (defined-type #f string-type-definition (list)))

  (define number-type
    (defined-type #f number-type-definition (list)))

  (define integer-type
    (defined-type number-type integer-type-definition (list)))

  (define fixnum-type
    (defined-type integer-type fixnum-type-definition (list)))

  (define (pair-type $first $second)
    (defined-type #f pair-type-definition (list $first $second)))

  (define (list-type $item)
    (recursive-type
      (union-type
        (list
          null-type
          (pair-type $item (variable-type 0))))))
)
