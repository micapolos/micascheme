(library (zx-next compiler stacked)
  (export
    stacked op op1 op2 peek
    const
    inc dec
    add sub and or xor
    neg cpl)
  (import (only (micascheme) define-keywords))

  (define-keywords
    stacked op op1 op2 peek
    const
    inc dec
    add sub and or xor
    neg cpl)
)
