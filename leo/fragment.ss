(library (leo fragment)
  (export
    fragment fragment? fragment-style fragment-ref
    inline-style inline-style?
    colon-style colon-style?
    block-style block-style?
    style-switch)
  (import (micascheme))

  (data*
    (fragment style ref)
    inline-style
    colon-style
    block-style)

  (enum (style inline-style colon-style block-style))
)
