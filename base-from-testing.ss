(library (base-from-testing)
  (export foo single?)
  (import (chezscheme))

  (define foo "foo")
  (define single? "single? override")
)