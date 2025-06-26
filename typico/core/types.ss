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

  (define type-type (gentype type))
  (define syntax-type (gentype syntax))
  (define datum-type (gentype datum))

  (define boolean-type (gentype boolean boolean? identity))
  (define integer-type (gentype integer integer? identity))
  (define char-type (gentype char char? identity))
  (define string-type (gentype string string? identity))
  (define bytevector-type (gentype bytevector bytevector?))

  (define u2-type (gentype u2 u2? identity))
  (define u3-type (gentype u3 u3? identity))
  (define u7-type (gentype u7 u7? identity))
  (define u8-type (gentype u8 u8? identity))
  (define u16-type (gentype u16 u16? identity))

  (define s8-type (gentype s8 s8? identity))

  (define list-type (forall-type 1 (gentype list)))

  (define (list-of-type $type)
    (application-type list-type (list $type)))
)
