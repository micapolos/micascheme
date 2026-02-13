(library (micalang base)
  (export)
  (import (micascheme))
  (export
    (import
      (except (micascheme) environment environment-symbols)
      (symbol)
      (prefix (only (scheme) environment) %))))
