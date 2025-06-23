(library (annotation)
  (export
    datum/annotation-expression-stripped
    datum/annotation-stripped
    datum/annotation-expression
    datum/annotation)
  (import (scheme) (switch) (syntax))

  (define (datum/annotation-expression-stripped $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation)
        (values
          (annotation-expression $annotation)
          (annotation-stripped $annotation)))
      ((else $datum)
        (values $datum $datum))))

  (define (datum/annotation-stripped $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation) (annotation-stripped $annotation))
      ((else $datum) $datum)))

  (define (datum/annotation-expression $datum/annotation)
    (switch $datum/annotation
      ((annotation? $annotation) (annotation-expression $annotation))
      ((else $datum) $datum)))

  (define-rule-syntax (datum/annotation obj)
    (syntax->datum/annotation #'obj))
)
