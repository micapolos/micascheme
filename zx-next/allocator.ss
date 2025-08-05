(library (zx-next allocator)
  (export
    allocator-size
    allocator-init
    allocator-init-tc
    allocator-alloc
    allocator-alloc-tc)
  (import
    (zx-next core)
    (zx-next alloc-pointer))

  ; Allocation happens in slot 7.

  (define-values
    (tag-mask  #b11100000)
    (size-mask #b00011111)
    (slot 7)
    (slot-tag #b11100000)
    (allocator-size 2))

  (define-proc (allocator-init hl)
    (input (hl - allocator))
    ; DE = empty alloc pointer
    (ld de #xe000)

    ; Save alloc pointer
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)

    ; Initalize first allcation entry
    (ex de hl)
    (alloc-pointer-init-tc hl))

  (define-proc (allocator-alloc hl bc)
    (input
      (hl - allocator pointer)
      (bc - tagged size))
    (output
      (cf - 0 ok / 1 overflow)
      (de - allocated-address))

    ; de = alloc pointer
    (ld e (hl))
    (inc hl)
    (ld d (hl))

    (preserve (hl)
      ; hl = alloc pointer
      (ex de hl)

      ; HL = advanced alloc pointer
      ; DE = allocated pointer
      (alloc-pointer-alloc hl bc)

      ; BC = alloc-pointer
      (ld bc hl))

    ; Write back alloc pointer to allocator
    (ld (hl) b)
    (dec hl)
    (ld (hl) c)

    ; DE = allocated pointer
    (ret))
)
