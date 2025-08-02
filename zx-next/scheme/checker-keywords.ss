(library (zx-next scheme checker-keywords)
  (export byte word byte+ byte- lets)
  (import (only (micascheme) define-keywords))

  (define-keywords byte word + - lets)
)
