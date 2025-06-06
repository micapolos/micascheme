(import (scheme) (asm syntax) (asm asm))

(pretty-print
  (asm-bytevector
    (eq bar 3)
    (eq foo bar)
    (u8 0)
    (u8 10)))
