(library (typico core types)
  (export
    type-type
    boolean-type integer-type char-type string-type
    bytevector-type
    u2-type u3-type u7-type u8-type u16-type
    s8-type
    list-type
    list-of-type)
  (import (micascheme) (typico type))

  (define type-type (primitive-type (gensym) 'type))

  (define boolean-type (primitive-type (gensym) 'boolean))
  (define integer-type (primitive-type (gensym) 'integer))
  (define char-type (primitive-type (gensym) 'char))
  (define string-type (primitive-type (gensym) 'string))
  (define bytevector-type (primitive-type (gensym) 'bytevector))

  (define u2-type (primitive-type (gensym) 'u2))
  (define u3-type (primitive-type (gensym) 'u3))
  (define u7-type (primitive-type (gensym) 'u7))
  (define u8-type (primitive-type (gensym) 'u8))
  (define u16-type (primitive-type (gensym) 'u16))

  (define s8-type (primitive-type (gensym) 's8))

  (define list-type (primitive-type (gensym) 'list))

  (define (list-of-type $type)
    (application-type
      (forall-type 1 list-type)
      (list $type)))
)
