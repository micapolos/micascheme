(import
  (zx-next test)
  (zx-next alloc-banked-pointer)
  (zx-next mem)
  (zx-next mmu)
  (zx-next tagged)
  (zx-next bank-alloc))

(define-fragment bank (db 0))

(test
  ; We know that the first two banks are 1 and 2, but it may not always be true.
  ; Update bank-alloc to allow querying the next available bank.
  (mmu 7 #x01)
  (mem-fill #xe000 #x2000 #xbb)
  (mmu 7 #x02)
  (mem-fill #xe000 #x2000 #xbb)

  (case alloc-banked-pointer-new-bank-alloc
    (alloc-banked-pointer-new-bank-alloc #xff #xf800 (tagged-word #xa0 #x17fd))
    (assert nc)
    (assert a #x01)
    (assert hl #xf800)
    (assert de #xe003)
    (mmu 7 #x01)
    (assert-word (#xe001) (tagged-word #xa0 #x17fd))
    (assert-word (#xf800) #x00bb))

  (case alloc-banked-pointer-same-bank-alloc
    (alloc-banked-pointer-alloc #x01 #xf800 (tagged-word #xa0 #x00fe))
    (assert nc)
    (assert a #x01)
    (assert hl #xf900)
    (assert de #xf802)
    (mmu 7 #x01)
    (assert-word (#xf800) (tagged-word #xa0 #x00fe))
    (assert-word (#xf900) #x00bb))

  (case alloc-banked-pointer-alloc/bank-overflow
    (alloc-banked-pointer-alloc #x01 #xf900 (tagged-word #xa0 #x17fd))
    (assert nc)
    (assert a #x02)
    (assert hl #xf800)
    (assert de #xe003)
    (mmu 7 #x01)
    (assert-byte (#xe000) #x02)
    (mmu 7 #x02)
    (assert-byte (#xe000) #xff)
    (assert-word (#xe001) (tagged-word #xa0 #x17fd))
    (assert-word (#xf800) #x00bb))

  (case banked-pointer->alloc-banked-pointer
    (banked-pointer->alloc-banked-pointer #x01 #xe003)
    (assert a #x01)
    (assert hl #xe001)
    (mmu 7)
    (assert a #x01))

  (case alloc-banked-pointer-next/same-bank
    (alloc-banked-pointer-next #x01 #xe001)
    (assert nc)
    (assert e #x01)
    (assert hl #xf800))

  (case alloc-banked-pointer-next/next-bank
    (alloc-banked-pointer-next #x01 #xf900)
    (assert nc)
    (assert e #x02)
    (assert hl #xe001))

  (case alloc-banked-pointer-next/end
    (alloc-banked-pointer-next #x02 #xf800)
    (assert c))

  (bank-alloc-all)

  (case alloc-banked-pointer-alloc/out-of-memory
    (alloc-banked-pointer-alloc #x01 #xf800 (tagged-word #xa0 #x17fe))
    (assert c)
    (assert a #x01)
    (assert hl #xf800))
)
