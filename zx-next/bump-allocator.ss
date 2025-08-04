(library (zx-next bump-allocator)
  (export
    bump-allocator-size
    bump-allocator-init
    bump-allocator-alloc)
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
    (bump-allocator-size 2))

  (define-asm bump-allocator-init-proc
    (input (hl - bump allocator ptr))
    (preserve (hl)
      (ld e slot-tag)
      (call bump-pointer-init)
      (ex de hl))
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ret))

  (define-ops (keywords hl)
    ((bump-allocator-init hl)
      (call bump-allocator-init-proc))
    ((bump-allocator-init ptr)
      (ld hl ptr)
      (bump-allocator-init hl)))

  (define-fragment bump-allocator-alloc-proc
    (input
      (hl - bump-allocator pointer)
      (bc - size in bits 12 ... 0)
      (a - tag in bits 7 ... 5))
    (output
      (cf - 0 ok / 1 overflow)
      (de - allocated-address))
    (preserve (hl)
      ; hl = bump pointer
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (ex de hl)

      ; E = slot-tag
      (ld e slot-tag)

      ; D = tag
      (ld d a)

      ; HL = advanced bump pointer
      ; DE = allocated pointer
      (call bump-pointer-alloc)

      ; BC = bump-pointer
      (ld bc hl))

    ; Write back bump pointer to bump-allocator
    (ld (hl) b)
    (dec hl)
    (ld (hl) c)

    ; DE = allocated pointer
    (ret))

  (define-ops (keywords hl bc a)
    ((bump-allocator-alloc hl bc a)
      (call bump-allocator-alloc-proc))
    ((bump-allocator-alloc ptr size tag)
      (ld hl ptr)
      (ld bc size)
      (ld a tag)
      (bump-allocator-alloc hl bc a)))
)
