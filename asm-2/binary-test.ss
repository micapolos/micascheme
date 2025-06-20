(import (micascheme) (asm-2 binary))

(check
  (equal?
    (binary->bytevector (db-binary #x10 #'x))
    (bytevector #x10)))

(check
  (equal?
    (binary->bytevector (dw-binary #x1234 #'x))
    (bytevector #x34 #x12)))

(check (raises (binary->bytevector (db-binary #x100 #'x))))
(check (raises (binary->bytevector (dw-binary #x10000 #'x))))
