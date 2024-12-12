(import (scheme) (check) (bytevector))

(check-equal?
  (u16-bytevector #x1234 (endianness little))
  (bytevector #x34 #x12))

(check-equal?
  (u16-bytevector #x1234 (endianness big))
  (bytevector #x12 #x34))
