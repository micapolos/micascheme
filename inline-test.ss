(import (scheme) (check) (inline))

; === inline

(check
  (equal?
    (expand
      `(inline
        (import (scheme) (emu math))
        #`(($primitive 3 bytevector) #,(u8+ 1 2) 10))
      (environment '(scheme) '(inline)))
    `(($primitive 3 bytevector) 3 10)))

; === inline-datum

(check
  (equal?
    (expand
      '(inline-datum
        (import (scheme) (emu math))
        (map (lambda (x) (u8+1 x)) '(1 2 3)))
      (environment '(scheme) '(inline)))
    '(2 3 4)))

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

; === inline-vector

(check
  (equal?
    (expand
      '(inline-vector
        (import (scheme))
        (let ()
          (define $vector (make-vector 4 #`(lambda () "null")))
          (vector-set! $vector 0 #`(lambda () "0"))
          (vector-set! $vector 2 #`(lambda () "2"))
          (vector-set! $vector 3 #`(#,(vector-ref $vector 0) #,(vector-ref $vector 2)))
          $vector))
      (environment '(scheme) '(inline)))
    '(($primitive 3 vector)
      (lambda () "0")
      (lambda () "null")
      (lambda () "2")
      ((lambda () "0") (lambda () "2")))))

