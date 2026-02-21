(library (leo2 base)
  (export)
  (import (scheme))
  (export
    (import
      (except (micascheme)
        compile
        expand
        atom?
        indexed indexed? indexed-index indexed-value))))
