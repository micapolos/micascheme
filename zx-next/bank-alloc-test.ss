(import
  (zx-next test)
  (only (zx-next bank) bank-fill)
  (zx-next bank-table)
  (zx-next bank-alloc))

(define-fragments
  (bank (db 0))
  (banks (ds 256))
  (bank-count (db 0))
  (free-bank-count (db 0)))

(test
  (write-bank-table)

  (case count-free-banks
    (count-free-banks)
    (ld hl free-bank-count)
    (ld (hl) a))

  (case bank-alloc
    (bank-alloc)
    (assert nc)
    (ld hl bank)
    (ld (hl) a)
    (bank-reserved? a)
    (assert nz))

  (write-bank-table)

  (case bank-reserved?
    (ld hl bank)
    (ld a (hl))
    (bank-reserved? a)
    (assert nz))

  (case one-less-free-bank
    (count-free-banks)
    (ld hl free-bank-count)
    (ld b (hl))
    (dec b)
    (cp b)
    (assert z))

  (case bank-free
    (ld hl bank)
    (ld a (hl))
    (bank-free a))

  (write-bank-table)

  (case bank-reserved?
    (ld hl bank)
    (ld a (hl))
    (bank-reserved? a)
    (assert z))

  (case one-more-free-bank
    (count-free-banks)
    (ld hl free-bank-count)
    (cp (hl))
    (assert z))

  (write-bank-table)

  (case alloc-all
    (ld b 0)
    (ld hl banks)
    (loop
      (preserve (bc hl) (bank-alloc))
      (when nc
        (ld (hl) a)
        (inc hl)
        (inc b)

        (preserve (hl bc af)
          (bank-reserved? a)
          (assert nz))

        (preserve (bc hl)
          (ld l #xbb)
          (bank-fill a l)
          (write #\.))

        (rcf))
      (while nc))
    (ld hl bank-count)
    (ld (hl) b)
    (writeln))

  (write-bank-table)

  (case check-no-free-banks
    (count-free-banks)
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
          (bank-reserved? a)
          (assert nz))

        (preserve (hl bc af) (bank-free a))

        (preserve (hl bc af)
          (bank-reserved? a)
          (assert z))

        (write #\.)))
    (writeln))

  (write-bank-table)

  (case check-all-free-banks
    (count-free-banks)
    (ld hl free-bank-count)
    (cp (hl))
    (assert z))
)
