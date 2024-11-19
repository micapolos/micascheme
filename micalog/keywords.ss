(library (micalog keywords)
  (export
    module input output internal
    define on cond else
    register
    expr
    append slice
    + -
    and or not
    reg reg-ref)
  (import (only (micascheme) define-aux-keywords))

  (define-aux-keywords
    module input output internal
    define on cond else
    register
    expr
    append slice
    + -
    and or not
    reg reg-ref)
)
