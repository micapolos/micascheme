(library (micac keywords)
  (export
    const var
    set
    ref &ref
    while defer break-if cast then
    macro extern return)
  (import (syntax))

  (define-keywords
    const var
    set
    ref &ref
    while defer break-if cast then
    macro extern return)
)
