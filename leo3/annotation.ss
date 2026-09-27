(library (leo3 annotation)
  (export
    annotation-leo?
    leo-annotation?)
  (import (scheme))

  (define (annotation-leo? $annotation)
    (source-object-leo? (annotation-source-object $annotation)))

  (define (leo-annotation? $annotation)
    (and
      (annotation? $annotation)
      (annotation-leo? $annotation)))
)
