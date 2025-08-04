(import (zx-next test) (zx-next terminal) (zx-next bank) (zx-next mem) (zx-next mmu))

(define-fragments
  (banks (ds 256))
  (bank-count (db 0))
  (free-bank-count (db 0)))

(test
  (case init
    (call banks-init))

  (case free-banks
    (load-free-banks a)
    (assert a #xd0))

  (case free-bank-0?
    (ld a #x00)
    (call bank-free?)
    (assert nz))

  (case free-bank-0f?
    (ld a #x0f)
    (call bank-free?)
    (assert nz))

  (case free-bank-10?
    (ld a #x10)
    (call bank-free?)
    (assert z))

  (case free-bank-df?
    (ld a #xd0)
    (call bank-free?)
    (assert z))

  (case free-bank-e0?
    (ld a #xe0)
    (call bank-free?)
    (assert nz))

  (case alloc
    (ld a #x34)  ; some type
    (call bank-alloc)
    (assert nc)
    (assert a #x10))

  (case free-banks
    (load-free-banks a)
    (assert a #xcf))

  (case free-bank-1?
    (ld a #x01)
    (call bank-free?)
    (assert nz))

  (case alloc
    (ld a #x34)  ; some type
    (call bank-alloc)
    (assert nc)
    (assert a #x11))

  (case free-banks
    (load-free-banks a)
    (assert a #xce))

  (case free-bank-2?
    (ld a #x02)
    (call bank-free?)
    (assert nz))

  (case alloc-all
    (load-free-banks a)
    (ld hl free-bank-count)
    (ld (hl) a)

    (ld b 0)
    (ld hl banks)
    (loop
      (ld a #x34)
      (preserve (bc hl) (call bank-alloc))
      (if nc
        (then
          (mmu 7 a)
          (ld (hl) a)
          (inc hl)
          (inc b)
          (preserve (bc hl)
            (ld l #xbb)
            (bank-fill a l)
            (write #\.))
          (rcf))
        (else (scf)))
        (while nc))
    (ld hl bank-count)
    (ld (hl) b)
    (writeln))

  (case free-all
    (ld hl bank-count)
    (ld b (hl))
    (ld hl banks)
    (loop-djnz
      (ld a (hl))
      (inc hl)
      (preserve (hl bc)
        (call bank-dealloc)
        (write #\.)))
    (writeln)

    (load-free-banks a)
    (ld hl free-bank-count)
    (cp (hl))
    (assert z))
)
