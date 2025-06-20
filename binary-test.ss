(import (scheme) (check) (binary))

(check
  (equal?
    (binary->bytevector (u8-binary #x10))
    (bytevector #x10)))

(check
  (equal?
    (binary->bytevector (u16-binary #x1234 'little))
    (bytevector #x34 #x12)))

(check
  (equal?
    (binary->bytevector
      (binary-append
        (u8-binary #x10)
        (u16-binary #x3020 'little)))
    (bytevector #x10 #x20 #x30)))
