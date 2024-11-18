(library (micalog keywords)
  (export
    expr
    append slice
    + -
    and or not
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    expr
    append slice
    + -
    and or not
    reg reg-ref)
)
