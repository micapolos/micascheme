(library (zx-next banked-pointer)
  (export
    banked-pointer-page-in
    banked-pointer-page-in-tc
    banked-pointer-page-in-proc

    write-banked-pointer
    write-banked-pointer-tc
    write-banked-pointer-proc)
  (import
    (zx-next core)
    (zx-next mmu)
    (zx-next write))

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

  (define-proc (write-banked-pointer a hl)
    (preserve (hl)
      (call write-byte)
      (write #\:))
    (jp write-word))
)
