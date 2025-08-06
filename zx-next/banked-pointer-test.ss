(import
  (zx-next test)
  (zx-next banked-pointer)
  (zx-next mmu))

(test
  (case banked-pointer-page-in-0
    (banked-pointer-page-in #x40 #x1234)
    (mmu 0)
    (assert a #x40))

  (case banked-pointer-page-in-7
    (banked-pointer-page-in #x40 #xf234)
    (mmu 7)
    (assert a #x40)))
