(import (scheme) (check) (syntax) (binary))

(define-rule-syntax (check-binary $binary $bytevector)
  (check (equal? (binary->bytevector $binary) $bytevector)))

(check-binary
  (u8-binary #x10)
  (bytevector #x10))

(check-binary
  (u16-binary #x1234 'little)
  (bytevector #x34 #x12))

(check-binary
  (u16-binary #x1234 'big)
  (bytevector #x12 #x34))

(check-binary
  (utf8-binary "foo")
  (string->utf8 "foo"))

(check-binary
  (bytevector-binary (bytevector 1 2 3))
  (bytevector 1 2 3))

(check-binary
  (binary-append
    (u8-binary #x10)
    (u16-binary #x3020 'little))
  (bytevector #x10 #x20 #x30))
