(import (micascheme) (simplang expand))

(parameterize
  ((current-expand simplang-expand))
  (load "simplang/demo.ss"))
