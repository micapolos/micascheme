(library (micalog keywords)
  (export
    micalog
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
    micalog
    module input output internal
    define on cond else
    register
    expr
    append slice
    + -
    and or not
    reg reg-ref)
)
