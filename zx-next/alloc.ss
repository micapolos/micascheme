(library (zx-next alloc)
  (export
    alloc
    alloc-tc)
  (import
    (zx-next core)
    (zx-next banked-allocator)
    (zx-next throw))

  (define-fragment banked-allocator (db #xff #x00 #xe0))

  (define-proc (alloc bc)
    (input
      (bc tagged size))
    (output
      (throws out-of-memory)
      (ehl pointer)
      (mmu paged-in))

    (ld hl banked-allocator)

    ; A - bank, DE - address
    (banked-allocator-alloc hl bc)
    (when c (throw))

    ; Result in EHL
    (ex de hl)
    (ld e a)
    (ret))
)
