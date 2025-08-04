(library (zx-next allocator)
  (export
    allocator-size
    allocator-init
    allocator-init-tc
    allocator-alloc
    allocator-alloc-tc)
  (import
    (zx-next core)
    (zx-next bump-pointer))

  ; Allocation happens in slot 7.

  (define-values
    (tag-mask  #b11100000)
    (size-mask #b00011111)
    (slot 7)
    (slot-tag #b11100000)
    (allocator-size 2))

  (define-proc (allocator-init hl)
    (input (hl - allocator))
    ; DE = empty bump pointer
    (ld de #xe000)

    ; Save Bump pointer
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)

    ; Initalize first allcation entry
    (ex de hl)
    (bump-pointer-init-tc hl))

  (define-proc (allocator-alloc hl bc)
    (input
      (hl - allocator pointer)
      (bc - tagged size))
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

      ; HL = advanced bump pointer
      ; DE = allocated pointer
      (bump-pointer-alloc hl bc)

      ; BC = bump-pointer
      (ld bc hl))

    ; Write back bump pointer to allocator
    (ld (hl) b)
    (dec hl)
    (ld (hl) c)

    ; DE = allocated pointer
    (ret))
)
