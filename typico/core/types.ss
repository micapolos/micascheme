(library (typico core types)
  (export
    type-type
    datum-type
    boolean-type integer-type char-type string-type
    bytevector-type
    u2-type u3-type u7-type u8-type u16-type
    s8-type
    list-of-type
    list-of-kind)
  (import (micascheme) (typico type) (asm u))

  (define-type scheme)

  (define-type type)
  (define-type datum)

  (define-type boolean boolean? identity)
  (define-type integer (and? integer? exact?) identity)
  (define-type char char? identity)
  (define-type string string? identity)
  (define-type bytevector bytevector?)

  (define-type u2 u2? identity)
  (define-type u3 u3? identity)
  (define-type u7 u7? identity)
  (define-type u8 u8? identity)
  (define-type u16 u16? identity)

  (define-type s8 s8? identity)

  (define list-of-kind (forall-type 1 (gentype list-of)))

  (define (list-of-type $type)
    (application-type list-of-kind (list $type)))
)
