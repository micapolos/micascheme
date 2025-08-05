(library (zx-next banked-alloc-pointer)
  (export)
  (import
    (zx-next core)
    (zx-next alloc-pointer))

  ; Banked alloc pointer consists of 8-byte bank and address.
  ; It's passed in A HL registers.

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

    ; A = slot
    (ex af)
    (ld a h)
    (rlca)
    (rlca)
    (rlca)
    (and #x7)

    ; mmu in correct slot
    (dispatch
      (begin (ex af) (nextreg #x40 a))
      (begin (ex af) (nextreg #x41 a))
      (begin (ex af) (nextreg #x42 a))
      (begin (ex af) (nextreg #x43 a))
      (begin (ex af) (nextreg #x44 a))
      (begin (ex af) (nextreg #x45 a))
      (begin (ex af) (nextreg #x46 a))
      (begin (ex af) (nextreg #x47 a)))

    ; Allocate in that bank
    (preserve (hl bc) (alloc-pointer-alloc hl bc))

    ; Allocation OK? Return.
    (ret nc)

    ; Out of memory in this bank, allocate new one and repeat
    (preserve (hl bc) (bank-alloc))

    ; No more banks? return with out-of-memory
    (ret c)

    ; mmu in correct slot
    (dispatch
      (begin (ex af) (nextreg #x40 a))
      (begin (ex af) (nextreg #x41 a))
      (begin (ex af) (nextreg #x42 a))
      (begin (ex af) (nextreg #x43 a))
      (begin (ex af) (nextreg #x44 a))
      (begin (ex af) (nextreg #x45 a))
      (begin (ex af) (nextreg #x46 a))
      (begin (ex af) (nextreg #x47 a)))

    (alloc-pointer-alloc-tc hl bc))
)
