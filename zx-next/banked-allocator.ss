(library (zx-next banked-allocator)
  (export banked-allocator-init)
  (import
    (zx-next core)
    (zx-next allocator)
    (zx-next bank))

  (define-struct banked-allocator
    (current-bank db)
    (allocator    dw)
    (first-bank   db))

  (define-ops (keywords hl)
    ((banked-allocator-init hl)
      (call banked-allocator-init-proc))
    ((banked-allocator-init ptr)
      (ld hl ptr)
      (banked-allocator-init hl)))

  (define-fragment banked-allocator-init-proc
    (input (hl banked-allocator))

    ; a = allocated bank
    (preserve (hl) (call bank-alloc))
    (when c (ret))

    ; save current and first bank
    (ld (hl) a)
    (inc hl)
    (inc hl)
    (inc hl)
    (ld (hl) a)

    ; init allocator
    (dec hl)
    (dec hl)
    (allocator-init hl)
    (ret))
)
