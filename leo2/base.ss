(library (leo2 base)
  (export)
  (import (scheme))
  (export
    (import
      (except (micascheme)
        compile
        expand
        indexed indexed? indexed-index indexed-value))))
