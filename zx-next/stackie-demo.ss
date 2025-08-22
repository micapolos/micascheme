(import (zx-next demo) (zx-next stackie))

(demo
  (const-8 #\c)
  (dup-8)
  (dec-8)
  (dup-8)
  (dec-8)
  (putc)
  (putc)
  (putc)
  (const-8 #\return)
  (putc))
