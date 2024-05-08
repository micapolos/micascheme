(import (scheme) (check) (foreign))

; === with-locked-object

(check
  (equal?
    (with-locked-object ($string "foo")
      (string-append $string "!"))
    "foo!"))

; === with-object->reference-address

(check
  (integer?
    (with-object->reference-address
      ($addr (bytevector 3 4 5))
      $addr)))

; === foreign-string-length

(check
  (equal?
    (with-object->reference-address ($address (bytevector 0))
      (foreign-string-length $address))
    0))

(check
  (equal?
    (with-object->reference-address ($address (bytevector 1 2 3 0))
      (foreign-string-length $address))
    3))

; === foreign-string

(check
  (equal?
    (with-object->reference-address ($address (string->utf8 "\x0;"))
      (foreign-string $address))
    ""))

(check
  (equal?
    (with-object->reference-address ($address (string->utf8 "Hellą\x1234;!\x0;"))
      (foreign-string $address))
    "Hellą\x1234;!"))
