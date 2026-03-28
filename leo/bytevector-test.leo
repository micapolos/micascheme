import leo scheme

define bytevector-16-32 bytevector: 16, 32

check equal?
  bytevector-u8-ref: bytevector-16-32, 0
  16

check equal?
  bytevector-u8-ref: bytevector-16-32, 1
  32
