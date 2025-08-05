(import
  (zx-next test)
  (zx-next banked-alloc-pointer)
  (zx-next mem)
  (zx-next mmu))

(test
  (mmu 7 #x40)
  (mem-fill #xe000 #x2000 #xbb)

  (case banked-alloc-pointer-init
    (banked-alloc-pointer-init #x40 #xe000)
    (mmu 7 #x40)
    (assert-word (#xe000) #x00bb)))
