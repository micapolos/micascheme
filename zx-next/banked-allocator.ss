(library (zx-next banked-allocator)
  (export
    banked-allocator-size
    banked-allocator-init
    banked-allocator-init-tc
    banked-allocator-alloc
    banked-allocator-alloc-tc)
  (import
    (zx-next core)
    (zx-next alloc-banked-pointer))

  (define-values (banked-allocator-size 3))

  (define-proc (banked-allocator-init hl de)
    (input (hl banked-allocator) (de start-pointer))
    (ld (hl) #xff)   ; previous bank
    (inc hl)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d))

  (define-proc (banked-allocator-alloc hl bc)
    (input
      (hl - allocator pointer)
      (bc - tagged size))
    (output
      (cf - 0 ok / 1 overflow)
      (de - allocated-address))

    ; a, de = alloc banked pointer
    (preserve (hl)
      (ld a (hl))
      (inc hl)
      (ld e (hl))
      (inc hl)
      (ld d (hl))

      ; hl = banked alloc pointer
      (ex de hl)

      ; A HL = advanced alloc banked pointer
      ; DE = allocated pointer
      (cp #xff)
      (if z
        (then (alloc-banked-pointer-new-bank-alloc a hl bc))
        (else (alloc-banked-pointer-alloc a hl bc)))

      ; A, BC = alloc-banked-pointer
      (ld bc hl))

    ; Write back alloc pointer to allocator
    (ld (hl) a)
    (inc hl)
    (ld (hl) c)
    (inc hl)
    (ld (hl) b)

    ; A, DE = allocated bank and pointer
    (ret))
)
