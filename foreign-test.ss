(import (scheme) (check) (foreign))

(check
  (equal?
    (foreign-string (object->reference-address (string->utf8 "Hello!")))
    "Hello!"))
