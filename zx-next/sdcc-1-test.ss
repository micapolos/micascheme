(import (zx-next test) (zx-next sdcc-1))

(test
  (case ld-byte (ld-byte #x12)   (byte-assert #x12))
  (case ld-word (ld-word #x1234) (word-assert #x1234)))
