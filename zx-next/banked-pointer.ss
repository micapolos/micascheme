(library (zx-next banked-pointer)
  (export
    banked-pointer-page-in
    banked-pointer-page-in-tc)
  (import
    (zx-next core)
    (zx-next mmu))

  (define-proc (banked-pointer-page-in a hl)
    (input (a bank) (hl address))
    (output (mmu paged-in) (hl preserved))

    ; E = bank
    (ld e a)

    ; A = slot
    (ld a h)
    (rlca)
    (rlca)
    (rlca)
    (and #x7)

    ; mmu in correct slot
    (mmu a e)
    (ret))
)
