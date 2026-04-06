import
  only
    equal?, *
    ftype-sizeof, make-ftype-pointer
    make-immobile-bytevector, bytevector-u8-set!
    object->reference-address
    from chezscheme
  leo check
  leo define
  leo ftype

define-ftype
  point struct
    x unsigned-8
    y unsigned-8

check equal?: ftype-sizeof point, 2

define bytevector make-immobile-bytevector *: ftype-sizeof point, 2

define point-pointer
  make-ftype-pointer: point, object->reference-address bytevector

ftype-set!: point, point-pointer, x, 10
ftype-set!: point, point-pointer, y, 20
ftype*-set!: point, point-pointer, 1, x, 30
ftype*-set!: point, point-pointer, 1, y, 40

check equal?
  ftype-ref: point, point-pointer, x
  10

check equal?
  ftype-ref: point, point-pointer, y
  20

check equal?
  ftype*-ref: point, point-pointer, 0, x
  10

check equal?
  ftype*-ref: point, point-pointer, 0, y
  20

check equal?
  ftype*-ref: point, point-pointer, 1, x
  30

check equal?
  ftype*-ref: point, point-pointer, 1, y
  40

check equal?
  ftype-any-ref: point, point-pointer, x
  10

check equal?
  ftype-any-ref: point, point-pointer, y
  20

check equal?
  ftype*-any-ref: point, point-pointer, 0, x
  10

check equal?
  ftype*-any-ref: point, point-pointer, 0, y
  20

check equal?
  ftype*-any-ref: point, point-pointer, 1, x
  20

check equal?
  ftype*-any-ref: point, point-pointer, 1, y
  30
