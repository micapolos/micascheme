(import (micascheme))

(test
  (zx-next compiler stacked-asm)
  (zx-next compiler expr-stacked)
  (zx-next compiler expr-asm)
  ;(zx-next compiler named)
  )
