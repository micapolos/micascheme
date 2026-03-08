(library (annotation)
  (export
    datum/annotation-expression-stripped
    datum/annotation-stripped
    datum/annotation-expression
    datum/annotation
    fake-annotation
    annotation-cons
    append-annotation
    annotation/eof-stripped)
  (import
    (scheme)
    (switch)
    (syntax)
    (source-object))

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

  (define (annotation-cons $car $cdr)
    (make-annotation
      (cons $car $cdr)
      (append-source-object
        (annotation-source $car)
        (annotation-source $cdr))
      (cons
        (annotation-stripped $car)
        (annotation-stripped $cdr))))

  (define (append-annotation $annotation . $annotations)
    (make-annotation
      (apply append $annotation $annotations)
      (apply append-source-object
        (annotation-source $annotation)
        (map annotation-source $annotations))
      (apply append
        (annotation-stripped $annotation)
        (map annotation-stripped $annotations))))

  (define-rule-syntax (datum/annotation obj)
    (syntax->datum/annotation #'obj))

  (define-rule-syntax (fake-annotation obj)
    (make-annotation 'obj (make-source-object (source-file-descriptor "fake.ss" 0) 0 0) 'obj))

  (define (annotation/eof-stripped $annotation/eof)
    (switch $annotation/eof
      ((eof-object? $eof-object) $eof-object)
      ((else $annotation) (annotation-stripped $annotation))))
)
