(library (zx-next banked-alloc-pointer)
  (export
    banked-alloc-pointer-init
    banked-alloc-pointer-alloc)
  (import
    (zx-next core)
    (zx-next alloc-pointer)
    (zx-next bank-alloc)
    (zx-next mmu))

  ; Banked alloc pointer consists of 8-byte bank and address.
  ; It's passed in A HL registers.

  (define-proc (banked-alloc-pointer-init a hl)
    (input (a bank) (hl pointer))
    (inc hl)
    (ld (hl) 0)
    (ret))

  (define-proc (banked-alloc-pointer-alloc a hl bc)
    (input
      (a bank)
      (hl alloc-pointer)
      (bc tagged size))
    (output
      (fc 0 ok / 1 out-of-memory)
      (a allocated-bank)
      (de allocated-address)
      (mmu paged-in))

    ; E = bank
    (ld e a)

    ; A = slot
    (ld a h)
    (rlca)
    (rlca)
    (rlca)
    (and #x7)

    ; mmu in correct slot
    (preserve (bc) (mmu a e))

    ; Allocate in that bank
    (preserve (hl bc de) (alloc-pointer-alloc hl bc))

    ; Allocation OK? Return.
    (ret nc)

    ; Out of memory in this bank, allocate new one and repeat
    (preserve (hl bc de) (bank-alloc))

    ; No more banks? return with out-of-memory
    (ret c)

    ; mmu in correct slot
    (preserve (bc) (mmu a e))

    (alloc-pointer-alloc-tc hl bc))
)
