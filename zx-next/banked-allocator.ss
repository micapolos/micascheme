(library (zx-next banked-allocator)
  (export
    banked-allocator-size
    banked-allocator-first-bank
    banked-allocator-current-bank
    banked-allocator-allocator

    banked-allocator-init
    banked-allocator-alloc)
  (import
    (zx-next core)
    (zx-next allocator)
    (zx-next bank)
    (zx-next mmu))

  (define-values
    (banked-allocator-size         4)
    (banked-allocator-first-bank   0)
    (banked-allocator-current-bank 1)
    (banked-allocator-allocator    2))

  (define-struct banked-allocator
    (first-bank   db)
    (current-bank db)
    (allocator    dw))

  (define-ops (keywords hl)
    ((banked-allocator-init hl)
      (call banked-allocator-init-proc))
    ((banked-allocator-init ptr)
      (ld hl ptr)
      (banked-allocator-init hl))
    ((banked-allocator-alloc hl bc a)
      (call banked-allocator-alloc-proc))
    ((banked-allocator-alloc ptr size tag)
      (ld hl ptr)
      (ld bc size)
      (ld a tag)
      (banked-allocator-alloc hl bc a)))

  (define-fragment banked-allocator-init-proc
    (input (hl banked-allocator))

    ; a = allocated bank
    (preserve (hl) (call bank-alloc))
    (ret c)

    ; switch to allocated bank
    (mmu 7 a)

    ; save first and current bank
    (ld (hl) a)
    (inc hl)
    (ld (hl) a)
    (inc hl)

    ; init allocator
    (jp allocator-init-proc))

  (define-fragment banked-allocator-alloc-next-bank-proc
    (input (hl banked-allocator))

    ; a = allocated bank
    (preserve (hl) (call bank-alloc))
    (ret c)

    ; switch to allocated bank
    (mmu 7 a)

    ; save current bank
    (inc hl)
    (ld (hl) a)
    (inc hl)

    ; init allocator
    (jp allocator-init-proc))

  (define-fragment banked-allocator-alloc-proc
    (input (hl banked-allocator) (bc size) (a tag))

    (preserve (hl bc af)
      (preserve (af)
        ; switch to valid bank
        (inc hl)
        (ld a (hl))
        (inc hl)
        (mmu 7 a)

        ; hl = allocator
        (ld e (hl))
        (inc hl)
        (ld d (hl))
        (ex de hl))

      ; Allocate
      (break)
      (allocator-alloc hl bc a))
    (ret)

    ; Return when no bank overflow
    (ret nc)

    ; a = new bank
    (preserve (hl bc af) (call banked-allocator-alloc-next-bank-proc))
    (ret c)

    ; recursively allocate in new bank
    (inc hl)
    (inc hl)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (ex de hl)
    (allocator-alloc hl bc a)
    (ret))
)
