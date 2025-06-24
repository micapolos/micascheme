(library (annotation)
  (export
    datum/annotation-expression-stripped
    datum/annotation-stripped
    datum/annotation-expression
    datum/annotation
    fake-annotation)
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

  (define-rule-syntax (fake-annotation obj)
    (make-annotation 'obj (make-source-object (source-file-descriptor "fake.ss" 0) 0 0) 'obj))
)
