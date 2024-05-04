(import (scheme) (check) (inline))

(check
  (equal?
    (inline-bytevector
      (let ()
        (define $bytevector (make-bytevector 4 0))
        (bytevector-u8-set! $bytevector 0 10)
        (bytevector-u8-set! $bytevector 2 20)
        $bytevector))
    (bytevector 10 0 20 0)))
