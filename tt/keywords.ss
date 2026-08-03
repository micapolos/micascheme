(library (tt keywords)
  (export
    forall lambda pi quote
    boolean number char string datum
    if
    unchecked
    macro
    eq? ->datum)
  (import (syntax))

  (define-keywords
    forall lambda pi quote
    boolean number char string datum
    if
    unchecked
    macro
    eq? ->datum)
)
