(import (scheme) (check) (inline))

; === inline

(check
  (equal?
    (inline #`(bytevector (+ 1 2) 10))
    (bytevector 3 10)))

(check
  (equal?
    (inline (import (scheme))
      #`(bytevector (+ 1 2) 10))
    (bytevector 3 10)))

(check
  (equal?
    (inline
      (import (scheme) (emu math))
      #`(bytevector (u8+ 1 2) 10))
    (bytevector 3 10)))

; === inline-bytevector

(check
  (equal?
    (expand
      '(inline-bytevector
        (import (scheme) (emu math))
        (let ()
          (define $bytevector (make-bytevector 4 0))
          (bytevector-u8-set! $bytevector 0 10)
          (bytevector-u8-set! $bytevector 2 20)
          (bytevector-u8-set! $bytevector 3
            (u8+
              (bytevector-u8-ref $bytevector 0)
              (bytevector-u8-ref $bytevector 2)))
          $bytevector))
      (environment '(scheme) '(inline)))
    `(($primitive 3 bytevector) 10 0 20 30)))

