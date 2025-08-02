(library (zx-next scheme compiler-keywords)
  (export begin write write-stack arg top byte word symbol string byte+ byte- lets)
  (import (only (micascheme) define-keywords))

  (define-keywords
    begin write write-stack arg top byte word symbol string byte+ byte- lets)
)
