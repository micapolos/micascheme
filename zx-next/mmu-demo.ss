(import (zx-next core) (zx-next mmu))

(run
  (mmu 7 a)
  (mmu 7 #x20)
  (break))
