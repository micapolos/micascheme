(library (zx-next mmu)
  (export mmu)
  (import
    (zx-next core)
    (zx-next dispatch))

  (define-ops (keywords a e)
    ((mmu a e)      (add #x50) (nextreg a e))
    ((mmu slot a)   (nextreg (fx+ #x50 slot) a))
    ((mmu slot n)   (nextreg (fx+ #x50 slot) n))

    ((mmu a)        (add #x50) (ld nextreg))
    ((mmu slot)     (ld a slot) (mmu a)))
)
