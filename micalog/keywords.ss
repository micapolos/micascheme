(library (micalog keywords)
  (export
    + - append
    and or not)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    + - append
    and or not)
)
