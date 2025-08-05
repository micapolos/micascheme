(import
  (zx-next test)
  (zx-next alloc-banked-pointer)
  (zx-next mem)
  (zx-next mmu)
  (zx-next tagged)
  (zx-next bank-alloc))

(define-fragment bank (db 0))

(test
  (mmu 7 #x40)
  (mem-fill #xe000 #x2000 #xbb)

  ; We know that this is the first available bank, but it may not always be true.
  ; Update bank-alloc to allow querying the next available bank.
  (mmu 7 #x01)
  (mem-fill #xe000 #x2000 #xbb)

  (case alloc-banked-pointer-init
    (alloc-banked-pointer-init #x40 #xf800)
    (mmu 7 #x40)
    (assert a #x40)
    (assert hl #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc-banked-pointer-alloc
    (alloc-banked-pointer-alloc #x40 #xe000 (tagged-word #xa0 #x17fe))
    (assert nc)
    (assert a #x40)
    (assert hl #xf800)
    (assert de #xe002)
    (mmu 7 #x40)
    (assert-word (#xe000) (tagged-word #xa0 #x17fe))
    (assert-word (#xf800) #x00bb))

  (case alloc-banked-pointer-alloc/bank-overflow
    (alloc-banked-pointer-alloc #x40 #xf800 (tagged-word #xa0 #x17fe))
    (assert nc)
    (assert a #x01)
    (mmu 7 a)
    (assert hl #xf800)
    (assert de #xe002)
    (assert-word (#xe000) (tagged-word #xa0 #x17fe))
    (assert-word (#xf800) #x00bb))

  (bank-alloc-all)

  (case alloc-banked-pointer-alloc/out-of-memory
    (alloc-banked-pointer-alloc #x01 #xf800 (tagged-word #xa0 #x17fe))
    (assert c)
    (assert a #x01)
    (assert hl #xf800))
)
