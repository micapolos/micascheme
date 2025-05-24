(library (sjasm lang)
  (export sjasm)
  (import (micascheme) (sjasm run) (sjasm string))
  (export (import (sjasm keywords)))
  (export (import (only (micascheme) +)))

  (define-rule-syntax (sjasm line ...)
    (sjasm-run (sjasm-string line ...)))
)
