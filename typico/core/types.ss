(library (typico core types)
  (export
    type-type
    datum-type
    boolean-type integer-type char-type string-type
    bytevector-type
    u2-type u3-type u7-type u8-type u16-type
    s8-type
    generic-list-type list-type list-item?
    generic-unsafe-type unsafe-type
    generic-optional-type optional-type)
  (import (micascheme) (typico type) (asm u))

  (define-type scheme)

  (define-type type)
  (define-type datum)

  (define-type boolean)
  (define-type integer)
  (define-type char)
  (define-type string)
  (define-type bytevector)

  (define-type u2)
  (define-type u3)
  (define-type u7)
  (define-type u8)
  (define-type u16)

  (define-type s8)

  (define-type (list item))
  (define-type (unsafe type))
  (define-type (optional type))

  (define (list-item? $type)
    (switch? $type
      ((application-type? $application-type)
        (and
          (type=? (application-type-type $application-type) generic-list-type)
          (car (application-type-args $application-type))))))

)
