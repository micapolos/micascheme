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

(check-equal?
  (asm-bytevector (dw #x1234 #x5678))
  (bytevector #x34 #x12 #x78 #x56))

(check-equal?
  (asm-bytevector (db #x12 (dw #x3456) #x78))
  (bytevector #x12 #x56 #x34 #x78))
