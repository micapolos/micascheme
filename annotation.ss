(library (annotation)
  (export
    datum/annotation->expression-stripped
    datum/annotation->stripped
    datum/annotation->expression)
  (import (scheme) (switch))

  (define (datum/annotation->expression-stripped $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation)
        (values
          (annotation-expression $annotation)
          (annotation-stripped $annotation)))
      ((else $datum)
        (values $datum $datum))))

  (define (datum/annotation->stripped $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation) (annotation-stripped $annotation))
      ((else $datum) $datum)))

  (define (datum/annotation->expression $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation) (annotation-expression $annotation))
      ((else $datum) $datum)))
)
