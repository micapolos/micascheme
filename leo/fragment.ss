(library (leo fragment)
  (export
    fragment fragment? fragment-style fragment-ref
    inline-style colon-style block-style
    style-switch)
  (import (micascheme))

  (data*
    (fragment style ref)
    inline-style
    colon-style
    block-style)

  (enum (style inline-style colon-style block-style))
)
