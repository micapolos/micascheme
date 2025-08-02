(library (zx-next scheme compiler-keywords)
  (export begin write write-stack local byte word symbol string byte+ byte- lets with-locals)
  (import (only (micascheme) define-keywords))

  (define-keywords
    begin write write-stack local byte word symbol string byte+ byte- lets with-locals)
)
