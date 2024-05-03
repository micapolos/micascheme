(library (emu mem)
  (export define-mem mem-ref mem-read mem-write)
  (import (scheme) (syntax) (syntaxes) (lets) (emu internal))

  (define-internal mem-bytevector)

  (define-rules-syntaxes
    ((define-mem id size)
      (begin
        (define-rules-syntax
          ((id addr) (bytevector-u8-ref (mem-bytevector id) addr))
          ((id addr u8) (bytevector-u8-set! (mem-bytevector id) addr u8)))
        (define-mem-bytevector id (make-bytevector size 0))))

    ((mem-ref id) (mem-bytevector id))

    ((mem-read ref addr)
      (bytevector-u8-ref ref addr))

    ((mem-write ref addr u8)
      (bytevector-u8-set! ref addr u8)))
)
