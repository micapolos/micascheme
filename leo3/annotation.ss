(library (leo3 annotation)
  (export
    annotation-leo?
    leo-annotation?)
  (import
    (scheme)
    (leo3 source-object))

  (define (annotation-leo? $annotation)
    (source-object-leo? (annotation-source $annotation)))

  (define (leo-annotation? $annotation)
    (and
      (annotation? $annotation)
      (annotation-leo? $annotation)))
)
