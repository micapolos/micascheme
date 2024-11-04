(library (micac syntax)
  (export
    const var set ref &ref
    add sub
    shl shr
    while defer break-if cast)
  (import (micascheme))

  (define-aux-keywords const var set ref &ref add sub shl shr while defer break-if cast)
)
