(import
  (zx-next test)
  (zx-next alloc-banked-pointer)
  (zx-next mem)
  (zx-next mmu))

(test
  (mmu 7 #x40)
  (mem-fill #xe000 #x2000 #xbb)

  (case alloc-banked-pointer-init
    (alloc-banked-pointer-init #x40 #xe000)
    (mmu 7 #x40)
    (assert-word (#xe000) #x00bb)))
