(library (zx-next alloc-banked-pointer)
  (export
    alloc-banked-pointer-init
    alloc-banked-pointer-alloc)
  (import
    (zx-next core)
    (zx-next alloc-pointer)
    (zx-next bank-alloc)
    (zx-next mmu))

  ; Banked alloc pointer consists of 8-byte bank and address.
  ; It's passed in A HL registers.

  (define-proc (page-in a hl)
    (input (a bank) (hl address))
    (output (hl preserved) (de preserved))
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

  (define-proc (alloc-banked-pointer-init a hl)
    (input (a bank) (hl pointer))
    (output (a bank) (hl initialized pointer) (mmu paged-in))

    (preserve (af)
      (page-in a hl)

      ; Reset pointer
      (ld a h)
      (and #xe0)
      (ld h a)
      (ld l 0)

      (inc hl)
      (ld (hl) 0)
      (dec hl))

    (ret))

  (define-proc (alloc-banked-pointer-alloc a hl bc)
    (input
      (a bank)
      (hl banked-alloc-pointer)
      (bc tagged size))
    (output
      (fc 0 ok / 1 out-of-memory)
      (de allocated-address)
      (a advanced-bank)
      (hl advanced-banked-alloc-pointer)
      (mmu paged-in))

    ; Allocate in that bank
    (preserve (af hl bc)
      ; Page-in target bank.
      (preserve (bc) (page-in a hl))

      ; Allocate in target bank.
      (alloc-pointer-alloc hl bc)

      ; Allocation OK? Return.
      (when nc
        ; Return preserving A, HL, DE
        (pop bc)
        (pop bc)
        (pop af)
        (rcf)
        (ret)))

    ; Allocate new bank
    (push af)
    (push hl)
    (push bc)
    (bank-alloc)

    ; No more banks?
    (when c
      ; Return C, preserving A HL BC.
      (pop bc)
      (pop hl)
      (pop af)
      (scf)
      (ret))

    (pop bc)
    (pop hl)
    (pop de) ; preserve newly allocated bank in A

    ; Initialize alloc pointer in new bank.
    (preserve (bc)
      (alloc-banked-pointer-init a hl))

    ; Allocate in the new bank, preserving A
    (preserve (af)
      (alloc-pointer-alloc hl bc)
      (when c
        (pop af)
        (scf)
        (ret)))

    ; Success.
    (rcf)
    (ret))
)
