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
    (ld hl free-bank-count)
    (ld (hl) a))

  (case alloc-all
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

          (preserve (hl bc af)
            (call bank-free?)
            (assert nz))

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

  (case check-no-free-banks
    (load-free-banks a)
    (assert a 0))

  (case free-all
    (ld hl bank-count)
    (ld b (hl))
    (ld hl banks)
    (loop-djnz
      (ld a (hl))
      (inc hl)
      (preserve (hl bc)
        (preserve (hl bc af)
          (call bank-free?)
          (assert nz))

        (preserve (hl bc af) (call bank-dealloc))

        (preserve (hl bc af)
          (call bank-free?)
          (assert z))

        (write #\.)))
    (writeln))

  (case check-all-free-banks
    (load-free-banks a)
    (ld hl free-bank-count)
    (cp (hl))
    (assert z))
)
