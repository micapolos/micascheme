(import
  (zx-next test)
  (zx-next mmu))

(test
  (case mmu-slot-n
    (mmu 7 #x40)
    (mmu 7)
    (assert a #x40))

  (case mmu-slot-a
    (ld a #x41)
    (mmu 7 a)
    (mmu 7)
    (assert a #x41))

  (case mmu-a-e
    (ld a 7)
    (ld e #x42)
    (mmu a e)
    (ld a 7)
    (mmu a)
    (assert a #x42)))
