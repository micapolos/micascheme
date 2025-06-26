(library (typico core types)
  (export
    type-type
    syntax-type
    datum-type
    boolean-type integer-type char-type string-type
    bytevector-type
    u2-type u3-type u7-type u8-type u16-type
    s8-type
    list-type
    list-of-type)
  (import (micascheme) (typico type) (asm u))

  (define type-type (primitive-type (gensym) 'type))
  (define syntax-type (primitive-type (gensym) 'syntax))
  (define datum-type (primitive-type (gensym) 'datum))

  (define boolean-type (primitive-type (gensym) 'boolean boolean? identity))
  (define integer-type (primitive-type (gensym) 'integer integer? identity))
  (define char-type (primitive-type (gensym) 'char char? identity))
  (define string-type (primitive-type (gensym) 'string string? identity))
  (define bytevector-type (primitive-type (gensym) 'bytevector bytevector?))

  (define u2-type (primitive-type (gensym) 'u2 u2? identity))
  (define u3-type (primitive-type (gensym) 'u3 u3? identity))
  (define u7-type (primitive-type (gensym) 'u7 u7? identity))
  (define u8-type (primitive-type (gensym) 'u8 u8? identity))
  (define u16-type (primitive-type (gensym) 'u16 u16? identity))

  (define s8-type (primitive-type (gensym) 's8 s8? identity))

  (define list-type
    (forall-type 1 (primitive-type (gensym) 'list)))

  (define (list-of-type $type)
    (application-type list-type (list $type)))
)
