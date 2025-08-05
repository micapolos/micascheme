(import (zx-next core) (zx-next mmu))

(run
  (mmu 7 a)
  (mmu 7 #x20)

  (ld e 2)
  (ld a #x40)
  (mmu e a)

  (ld e 3)
  (ld a #x41)
  (mmu e a)

  (break))
