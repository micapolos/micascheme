import leo scheme

define bytevector-16-32 bytevector: 16, 32

check equal?
  bytevector-length with bytevector
  0

check equal?
  bytevector-length bytevector-16-32
  2

check equal?
  bytevector-u8-ref: bytevector-16-32, 0
  16

check equal?
  bytevector-u8-ref: bytevector-16-32, 1
  32
