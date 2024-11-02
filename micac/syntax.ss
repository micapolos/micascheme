(library (micac syntax)
  (export
    micac
    !=
    const var set ref &ref
    add sub
    shl shr
    while defer break-if)
  (import (micascheme))

  (define-aux-keywords micac != const var set ref &ref add sub shl shr while defer break-if)
)
