(library (emu mmu)
  (export define-mmu-8 mmu-8-bank mmu-8-addr)
  (import (scheme) (syntax) (syntaxes) (lets) (emu unit))

  (define-unit (mmu-8 bank-bits slot-bits)
    (bank-mask (fx1- (fxsll 1 bank-bits)))
    (slots-bytevector (make-bytevector (fxsll 1 slot-bits) 0)))

  (define-rules-syntaxes
    ((mmu-8-bank id slot)
      (bytevector-u8-ref (mmu-8-slots-bytevector id) slot))
    ((mmu-8-bank id slot bank)
      (bytevector-u8-set! (mmu-8-slots-bytevector id) slot bank))
    ((mmu-8-addr id addr)
      (lets
        (addr-var addr)
        (slot (fxsrl addr (mmu-8-bank-bits id)))
        (bank (bytevector-u8-ref (mmu-8-slots-bytevector id) slot))
        (fxior
          (fxsll bank (mmu-8-bank-bits id))
          (fxand addr (mmu-8-bank-mask id))))))
)
