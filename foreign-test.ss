(import (scheme) (check) (foreign))

; === with-locked-object

(check
  (equal?
    (with-locked-object "OK")
    "OK"))

(check
  (equal?
    (with-locked-object
      ($bytevector (bytevector 0 1 2))
      ($string "foo")
      `(,$string ,(bytevector-length $bytevector)))
    '("foo" 3)))

; === with-object->reference-address

(check
  (equal?
    (with-object->reference-address "foo")
    "foo"))

(check
  (not
    (zero?
      (with-object->reference-address
        ($addr-1 (bytevector 0 1 2))
        ($addr-2 (bytevector 3 4 5))
        (- $addr-1 $addr-2)))))

; === foreign-string-length

(check
  (equal?
    (with-object->reference-address
      ($address (bytevector 0))
      (foreign-string-length $address))
    0))

(check
  (equal?
    (with-object->reference-address
      ($address (bytevector 1 2 3 0))
      (foreign-string-length $address))
    3))

; === foreign-string

(check
  (equal?
    (with-object->reference-address
      ($address (string->utf8 "\x0;"))
      (foreign-string $address))
    ""))

(check
  (equal?
    (with-object->reference-address
      ($address (string->utf8 "Hellą\x1234;!\x0;"))
      (foreign-string $address))
    "Hellą\x1234;!"))
