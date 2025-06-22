(import (asm lang) (asm z80) (asm run) (asm asm) (asm asm-core))

(asm-run
  (di)
  (ld b 0)
  (ld a #b00000010)
  (loop (call step))
  (import (asm sandbox-step)))
