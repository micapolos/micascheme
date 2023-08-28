(library (leo indexed)
  (export)
  (import (micascheme))

  (data (native value))

  (data (variable index))
  (data (function arity body))
  (data (application target args))

  (data (index size value))
  (data (index-switch target cases))

  (data (tuple values))
  (data (tuple-get target index))
)