(library (zx-next compiler named-keywords)
  (export native lets)
  (import (only (micascheme) define-keywords))

  (define-keywords native lets)
)
