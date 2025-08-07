(library (zx-next scheme keywords)
  (export void begin lets cons car cdr quote write put-char put-string)
  (import (only (micascheme) define-keywords))

  (define-keywords void begin lets cons car cdr quote write put-char put-string)
)
