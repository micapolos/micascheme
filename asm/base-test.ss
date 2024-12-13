(import (micascheme) (asm syntax) (asm base))

(check-equal?
  (asm-bytevector (db #x10))
  (bytevector #x10))

(check-equal?
  (asm-bytevector (db #\space))
  (bytevector 32))

(check-equal?
  (asm-bytevector (db "foo" 32 "bar" #\!))
  (string->utf8 "foo bar!"))
