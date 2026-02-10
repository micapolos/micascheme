(library (micalang base)
  (export)
  (import (micascheme))
  (export
    (import
      (except (micascheme) pi environment environment-symbols)
      (prefix (only (scheme) environment) %))))
