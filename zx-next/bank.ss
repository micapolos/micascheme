(library (zx-next bank)
  (export
    bank-slot
    bank-base
    bank-size
    bank-fill)
  (import
    (zx-next core)
    (zx-next debug)
    (zx-next write)
    (zx-next mem)
    (zx-next mmu))

  (define-values
    (bank-slot 7)
    (bank-base #xe000)
    (bank-size #x2000))

  (define-proc (bank-fill a l)
    (mmu bank-slot a)
    (ld de bank-base)
    (ld bc bank-size)
    (ld a l)
    (mem-fill de bc a)
    (ret))
)
