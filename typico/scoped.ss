(library (typico scoped)
  (export
    scoped scoped? scoped-lookup scoped-ref
    scoped->datum)
  (import (micascheme))

  (data (scoped lookup ref))

  (define (scoped->datum $scoped)
    (datum/annotation-stripped (scoped-ref $scoped)))
)
