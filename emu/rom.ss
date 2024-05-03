(library (emu rom)
  (export define-rom define-rom)
  (import (scheme) (syntax) (syntaxes))

  (define-rules-syntax
    ((define-rom (id size write) body ...)
      (begin
        (define immutable-bytevector
          (let ()
            (define bytevector (make-bytevector size 0))
            (define-rule-syntax (write addr u8)
              (bytevector-u8-set! bytevector addr u8))
            body ...
            (bytevector->immutable-bytevector bytevector)))
        (define-rules-syntax
          ((id addr) (bytevector-u8-ref immutable-bytevector addr))
          ((id _ _) (void))))))
)
