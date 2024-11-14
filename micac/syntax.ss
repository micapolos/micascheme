(library (micac syntax)
  (export
    const var
    set
    ref &ref
    while defer break-if cast then ?
    macro extern)
  (import (micascheme))

  (define-aux-keywords
    const var
    set
    ref &ref
    while defer break-if cast then ?
    macro extern)
)
