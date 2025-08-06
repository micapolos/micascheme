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
    (zx-next bank-alloc)
    (zx-next mmu)
    (zx-next write)
    (zx-next debug))

  (define-values
    (bank-type  #x01)
    (max-bank   #x7f)
    (banked-allocator-size               (+ 3 128))  ; 131
    (banked-allocator-current-bank       0)          ; (db #xff)
    (banked-allocator-allocator          1)          ; (dw 0)
    (banked-allocator-banks              3))         ; (ds 128)

  (define-proc (banked-allocator-page-in hl)
    (ld a (hl))
    (cp #xff)
    (ret z)

    (inc hl)
    (inc hl)
    (inc hl)
    (ld a (hl))

    (mmu 7 a)
    (ret))

  (define-proc (banked-allocator-alloc-bank hl)
    (input (hl - banked-allocator))
    (output (fc - out of memory))

    ; Check free banks
    (ld a (hl))
    (cp max-bank)
    (when z (ret-c))

    ; a = allocated bank
    (ld a bank-type)
    (preserve (hl) (bank-alloc))
    (ret c)

    ; b = allocated bank
    (ld b a)

    ; c = increased current bank index
    (inc (hl))
    (ld c (hl))

    ; Page-in
    (ld a b)
    (mmu 7 a)

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
    (ld (hl) #xff)
    (inc hl)
    (ld (hl) #xff)
    (ret))

  (define-proc (banked-allocator-alloc hl bc)
    (input (hl banked-allocator) (bc size))
    (output (fc 0 ok / 1 out-of-memory) (de address) (a bank) (mmu 7 paged-in))

    ; Try to allocate
    ; DE = allocated address
    (preserve (hl bc)
      ; Page in
      (preserve (hl bc af) (banked-allocator-page-in hl))

      ; Allocate within paged-in bank
      (inc hl)
      (allocator-alloc hl bc))

    ; Return on success.
    (ret nc)

    ; Otherwise, allocate new bank.
    (preserve (hl bc) (banked-allocator-alloc-bank hl))
    (ret c)

    (inc hl)
    (allocator-alloc-tc hl bc))
)
