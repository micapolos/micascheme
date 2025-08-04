(library (zx-next allocator)
  (export
    allocator-size
    allocator-init
    allocator-init-tc
    allocator-init-full
    allocator-init-full-tc
    allocator-alloc
    allocator-alloc-tc)
  (import
    (zx-next core)
    (zx-next bump-pointer))

  ; Allocation happens in slot 7.
  ; Allocated pointers are 13-bits.

  (define-values
    (tag-mask  #b11100000)
    (size-mask #b00011111)
    (slot 7)
    (slot-tag #b11100000)
    (allocator-size 2))

  (define-proc (allocator-init hl)
    (input (hl - allocator))
    (preserve (hl)
      (ld e slot-tag)
      (bump-pointer-init e)
      (ex de hl))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ret))

  (define-proc (allocator-init-full hl)
    (input (hl - allocator))
    (xor a)
    (ld (hl) 0)
    (inc hl)
    (ld (hl) 0)
    (ret))

  (define-proc (allocator-alloc hl bc a)
    (input
      (hl - allocator pointer)
      (bc - size in bits 12 ... 0)
      (a - tag in bits 7 ... 5))
    (output
      (cf - 0 ok / 1 overflow)
      (de - allocated-address))
    ; de = bump pointer
    (ld e (hl))
    (inc hl)
    (ld d (hl))

    (preserve (hl)
      ; hl = bump pointer
      (ex de hl)

      ; E = slot-tag
      (ld e slot-tag)

      ; D = tag
      (ld d a)

      ; HL = advanced bump pointer
      ; DE = allocated pointer
      (bump-pointer-alloc hl de bc)

      ; BC = bump-pointer
      (ld bc hl))

    ; Write back bump pointer to allocator
    (ld (hl) b)
    (dec hl)
    (ld (hl) c)

    ; DE = allocated pointer
    (ret))
)
