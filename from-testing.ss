(library (from-testing)
  (export foo single?)
  (import (scheme))

  (define foo "foo")
  (define single? "single? override")
)
