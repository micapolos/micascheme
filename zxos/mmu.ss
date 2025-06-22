(library (zxos mmu)
  (export mmu)
  (import (asm asm) (asm lang) (asm z80) (asm std))

  (define-asm-rules (keywords a)
    ((mmu slot a)   (nextreg (+ #x50 (u3 slot)) a))
    ((mmu slot n)   (nextreg (+ #x50 (u3 slot)) n)))
)
