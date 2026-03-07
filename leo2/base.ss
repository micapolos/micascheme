(library (leo2 base)
  (export)
  (import (scheme))
  (export
    (import
      (except (micascheme)
        compile
        expand
        atom?
        define-monadic
        indexed indexed? indexed-index indexed-value
        error error?))))
