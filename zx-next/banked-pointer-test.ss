(import
  (zx-next test)
  (zx-next banked-pointer)
  (zx-next mmu)
  (zx-next write))

(test
  (case banked-pointer-page-in-0
    (banked-pointer-page-in #x40 #x1234)
    (mmu 0)
    (assert a #x40))

  (case banked-pointer-page-in-7
    (banked-pointer-page-in #x40 #xf234)
    (mmu 7)
    (assert a #x40))

  (case write-banked-pointer
    (write-banked-pointer #x40 #x1234)
    (writeln)))
