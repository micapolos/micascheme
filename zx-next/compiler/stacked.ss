(library (zx-next compiler stacked)
  (export stacked const op2 add)
  (import (only (micascheme) define-keywords))

  (define-keywords stacked const op2 add)
)
