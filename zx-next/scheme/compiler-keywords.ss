(library (zx-next scheme compiler-keywords)
  (export arg top byte word byte+ byte- lets)
  (import (only (micascheme) define-keywords))

  (define-keywords begin write write-stack arg top byte word byte+ byte- lets)
)
