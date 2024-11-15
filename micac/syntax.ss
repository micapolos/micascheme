(library (micac syntax)
  (export
    const var
    set
    ref &ref
    while defer break-if cast then
    macro extern)
  (import (syntax))

  (define-aux-keywords
    const var
    set
    ref &ref
    while defer break-if cast then
    macro extern)
)
