(library (zx-next mmu)
  (export mmu)
  (import (zx-next core))

  (define-ops (keywords a)
    ((mmu slot a)   (nextreg (fx+ #x50 slot) a))
    ((mmu slot n)   (nextreg (fx+ #x50 slot) n)))
)
