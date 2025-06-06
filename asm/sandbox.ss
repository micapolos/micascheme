(import (scheme) (asm syntax) (asm asm))

(pretty-print
  (asm-bytevector
    (eq foo 3)
    (u8 0)
    bar
    (u8 10)
    (u8 (+ foo bars))))
