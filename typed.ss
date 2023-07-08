(library (typed)
  (export)
  (import (micascheme) (type))

  (data (variable index))
  (data (abstraction params body))
  (data (application fn args))
)