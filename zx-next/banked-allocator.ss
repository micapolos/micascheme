(library (zx-next banked-allocator)
  (export
    banked-allocator-size
    banked-allocator-current-bank
    banked-allocator-allocator
    banked-allocator-banks

    banked-allocator-init
    banked-allocator-init-tc
    banked-allocator-alloc
    banked-allocator-alloc-tc)
  (import
    (zx-next core)
    (zx-next allocator)
    (zx-next bank)
    (zx-next mmu))

  (define-values
    (bank-type  #x01)
    (max-bank   #x7f)
    (banked-allocator-size               (+ 3 128))  ; 131
    (banked-allocator-current-bank       0)          ; (db #xff)
    (banked-allocator-allocator          1)          ; (dw 0)
    (banked-allocator-banks              3))         ; (ds 128)

  (define-proc (banked-allocator-check-free-banks hl)
    (ld a (hl))
    (cp max-bank)
    (when z (ret-c))
    (ret-nc))

  (define-op (banked-allocator-page-in hl)
    (ld a (hl))
    (mmu 7 a))

  (define-proc (banked-allocator-alloc-bank hl)
    (input (hl - banked-allocator))
    (output (fc - out of memory))

    ; a = allocated bank
    (ld a bank-type)
    (preserve (hl) (call bank-alloc))
    (ret c)

    ; b = allocated bank
    (ld b a)

    ; c = increased current bank index
    (inc (hl))
    (ld c (hl))

    ; Page-in
    (banked-allocator-page-in hl)

    ; Init allocator
    (inc hl)
    (preserve (bc hl) (allocator-init hl))

    ; Store allocated bank
    (inc hl)
    (inc hl)
    (ld a c)
    (add hl a)
    (ld a b)
    (ld (hl) a)

    (ret-nc))

  (define-proc (banked-allocator-init hl)
    (input (hl - banked allocator))
    (ld (hl) #xff)
    (inc hl)
    (allocator-init-full-tc hl))

  (define-proc (banked-allocator-alloc hl bc a)
    (input (hl banked-allocator) (bc size) (a tag))

    (preserve (bc af hl) (banked-allocator-check-free-banks hl))
    (ret c)

    (preserve (bc af hl) (banked-allocator-alloc-bank hl))
    (ret c)

    (banked-allocator-page-in hl)

    (inc hl)
    (allocator-alloc-tc hl bc a))
)
