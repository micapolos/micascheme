(library (micalog keywords)
  (export
    define on cond else
    expr
    append slice
    + -
    and or not
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    define on cond else
    expr
    append slice
    + -
    and or not
    reg reg-ref)
)
