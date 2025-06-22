(library (zxos mmu)
  (export mmu)
  (import (asm asm) (asm lang) (asm z80) (asm std))

  (define-asm-rules (keywords a)
    ((mmu n a)   (nextreg (+ #x50 (u3 n)) a))
    ((mmu n m)   (nextreg (+ #x50 (u3 n)) m)))
)
