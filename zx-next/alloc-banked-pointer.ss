(library (zx-next alloc-banked-pointer)
  (export
    alloc-banked-pointer-new-bank-alloc
    alloc-banked-pointer-new-bank-alloc-tc
    alloc-banked-pointer-alloc
    alloc-banked-pointer-alloc-tc
    alloc-banked-pointer-next
    alloc-banked-pointer-next-tc
    banked-pointer->alloc-banked-pointer
    banked-pointer->alloc-banked-pointer-tc)
  (import
    (zx-next core)
    (zx-next alloc-pointer)
    (zx-next banked-pointer)
    (zx-next bank-alloc)
    (zx-next mmu))

  ; Banked alloc pointer consists of 8-byte bank and address.
  ; It's passed in A HL registers.

  (define-proc (alloc-banked-pointer-new-bank-alloc a hl bc)
    (input (a bank) (hl pointer) (bc tagged-size))
    (output
      (c 0 ok / 1 out of memory)
      (a bank)
      (hl new-pointer)
      (mmu paged-in))

    ; Preserve current bank in E, #xff means there's no previous bank.
    (ld e a)

    ; Allocate new bank
    (preserve (bc de hl) (bank-alloc))

    ; No more banks?
    (when c
      (ld a e)  ; Restore current bank
      (scf)
      (ret))

    ; New bank in D
    (ld d a)

    ; Reset pointer
    (ld a h)
    (and #xe0)
    (ld h a)
    (ld l 0)

    ; Check if there is old bank.
    (ld a e)
    (cp #xff)

    ; If yes, store new bank in the first byte of old bank.
    (when nz
      ; Page in old bank
      (preserve (af hl bc de)
        (banked-pointer-page-in a hl))

      ; Store new bank
      (ld (hl) d))

    ; New bank id A
    (ld a d)

    ; Page-in to the new bank
    (preserve (af hl bc)
      (banked-pointer-page-in a hl))

    ; Initialize first byte in the new bank to #xff, meaning no next bank.
    (ld (hl) #xff)
    (inc hl)

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
    (preserve (af)
      ; Page-in target bank.
      (preserve (bc) (banked-pointer-page-in a hl))

      ; Allocate in target bank.
      (alloc-pointer-alloc hl bc)

      ; Allocation OK? Return.
      (when nc
        (pop af)
        (ret-nc)))

    ; Allocate in new bank
    (push af)
    (alloc-banked-pointer-new-bank-alloc a hl bc)
    (when c
      (pop af)
      (scf)
      (ret))

    (pop bc)  ; preserve new bank

    ; Success.
    (rcf)
    (ret))

  (define-proc (banked-pointer->alloc-banked-pointer a hl)
    (input
      (a bank)
      (hl pointer))
    (output
      (mmu paged-in)
      (a bank)
      (hl alloc-pointer))

    (banked-pointer-page-in a hl)
    (preserve (af) (pointer->alloc-pointer hl))
    (ret))

  (define-proc (alloc-banked-pointer-next e hl)
    (input
      (e bank)
      (hl pointer))
    (output
      (c 0 ok / 1 no next)
      (e next bank)
      (hl next pointer))

    (ld a e)
    (preserve (de) (banked-pointer-page-in a hl))
    (preserve (de) (alloc-pointer-next hl))
    (ret nc)

    (ld a h)
    (and #xe0)
    (ld h a)
    (ld l 0)
    (ld a (hl))
    (cp #xff)
    (when z (ret-c))
    (ld e a)
    (inc hl)
    (ret-nc))
)
