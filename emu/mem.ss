(library (emu mem)
  (export define-mem mem-read mem-write)
  (import (scheme) (syntax) (syntaxes) (lets))

  (define-rules-syntaxes
    ((define-mem id size)
      (begin
        (define-rules-syntax
          ((id addr) (mem-read id addr))
          ((id addr u8) (mem-write id addr u8)))
        (define bytevector (make-bytevector size 0))
        (define-mem-bytevector id bytevector)))
    ((mem-read id addr)
      (bytevector-u8-ref (mem-bytevector id) addr))
    ((mem-write id addr u8)
      (bytevector-u8-set! (mem-bytevector id) addr u8)))

  (define-rule-syntax (define-mem-bytevector id bytevector)
    (define-property id mem-bytevector #'bytevector))

  (define-lookup-syntax (mem-bytevector stx lookup)
    (syntax-case stx ()
      ((_ id)
        (lets
          (bytevector (lookup #'id #'mem-bytevector))
          (or bytevector (syntax-error #'id "not mem:"))))))
)
