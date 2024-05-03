(import (test))

(test
  (emu rom)
  (emu mem)
  (emu mmu)
  (emu z80 all)
  (emu zx-spectrum-48 all))
