(import (micascheme))

(test
  (zexy assemble)
  (zexy link)
  (zexy vectorize)
  (zexy asm-bytevector)
  (zexy env)
  (zexy asm)
  (zexy z80)
  (zexy z80-syntax)
  (zexy next))
