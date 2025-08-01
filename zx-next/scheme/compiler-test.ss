(import (micascheme) (zx-next scheme compiler) (u))

(check-compile-expression
  (u8 #x12)
  (u8 #f
    (%ld %e 0)
    (%byte-value #x12)
    (%push-value)))

(check-compile-expression
  (u16 #x1234)
  (u16 #f
    (%ld %e 0)
    (%word-value #x1234)
    (%push-value)))

(check-compile-expression
  (+ (u8 #x12) (u8 #x34))
  (u8 #f
    (%ld %e 2)
    (%byte-value #x34)
    (%push-value)
    (%ld %e 0)
    (%byte-value #x12)
    (%push-value)
    (%byte-add)))
