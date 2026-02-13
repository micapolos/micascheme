(library (micalang base)
  (export)
  (import (micascheme))
  (export
    (import
      (except (micascheme) environment environment-symbols)
      (prefix (only (scheme) environment) %))))
