(import (micascheme) (zx-next emu runtime))

(define mem (make-bytevector 4 0))
(define fmem (immutable-vector "foo" "bar" string-append))

(check (equal? (u8-ref 0) 0))
(check (equal? (u8-ref 1) 0))
(check (equal? (u8-ref 2) 0))
(check (equal? (u8-ref 3) 0))

(u8-set! 0 #x12)
(u8-set! 1 #x13)
(u8-set! 2 #x14)
(u8-set! 3 #x15)

(check (equal? (u8-ref 0) #x12))
(check (equal? (u8-ref 1) #x13))
(check (equal? (u8-ref 2) #x14))
(check (equal? (u8-ref 3) #x15))

(check (equal? (u16-ref 0 (endianness little)) #x1312))
(check (equal? (u16-ref 0 (endianness big)) #x1213))

(if (symbol=? (native-endianness) (endianness little))
  (check (equal? (u16-lo-ref 0) #x12))
  (check (equal? (u16-lo-ref 0) #x13)))

(if (symbol=? (native-endianness) (endianness little))
  (check (equal? (u16-hi-ref 0) #x13))
  (check (equal? (u16-hi-ref 0) #x12)))

(check (equal? (ref 0) "foo"))
(check (equal? (ref 1) "bar"))
(check (equal? ((ref 2) (ref 0) (ref 1)) "foobar"))
