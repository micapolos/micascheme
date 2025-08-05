(import (zx-next core) (zx-next mmu))

(run
  (mmu 7 a)
  (mmu 7 #x20)

  (ld a 2)
  (ld e #x40)
  (mmu a e)

  (ld a 3)
  (ld e #x41)
  (mmu a e)

  (break))
