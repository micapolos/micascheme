(library (emu mmu)
  (export define-mmu-8 mmu-8-bank mmu-8-addr)
  (import (scheme) (syntax) (syntaxes) (lets) (emu internal))

  (define-internal bank-bits)
  (define-internal bank-mask)
  (define-internal slot-bits)
  (define-internal slots-bytevector)

  (define-rule-syntax (define-mmu-8 id bank-bits slot-bits)
    (begin
      (define-aux-keyword id)
      (define-bank-bits id bank-bits)
      (define-bank-mask id (fx1- (fxsll 1 bank-bits)))
      (define-slot-bits id slot-bits)
      (define-slots-bytevector id (make-bytevector (fxsll 1 slot-bits) 0))))

  (define-rules-syntaxes
    ((mmu-8-bank id slot)
      (bytevector-u8-ref (slots-bytevector id) slot))
    ((mmu-8-bank id slot bank)
      (bytevector-u8-set! (slots-bytevector id) slot bank))
    ((mmu-8-addr id addr)
      (lets
        (addr-var addr)
        (slot (fxsrl addr (bank-bits id)))
        (bank (bytevector-u8-ref (slots-bytevector id) slot))
        (fxior
          (fxsll bank (bank-bits id))
          (fxand addr (bank-mask id))))))
)
