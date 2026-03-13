(library (annotation)
  (export
    datum/annotation-expression-stripped
    datum/annotation-stripped
    datum/annotation-expression
    datum/annotation
    fake-annotation
    annotation-cons
    append-annotation
    annotation/eof-stripped
    stripped-annotation
    list-annotation
    to-annotation
    datum/annotation=?
    annotation=?)
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

  (define (append-annotation $annotation . $annotations)
    (make-annotation
      (apply list
        (annotation-expression $annotation)
        (map annotation-expression $annotations))
      (apply append-source-object
        (annotation-source $annotation)
        (map annotation-source $annotations))
      (apply list
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

  (define (to-annotation $obj $source-object)
    (switch $obj
      ((annotation? $annotation) $annotation)
      ((pair? $pair) (annotation-cons $source-object (car $pair) (cdr $pair)))
      ((else $other) (stripped-annotation $other $source-object))))

  (define (stripped-annotation $stripped $source-object)
    (make-annotation $stripped $source-object $stripped))

  (define (annotation-cons $source-object $a $b)
    (make-annotation
      (cons (datum/annotation-stripped $a) (datum/annotation-stripped $b))
      $source-object
      (cons $a $b)))

  (define (list-annotation $list $source-object)
    (make-annotation
      $list
      $source-object
      (map datum/annotation-stripped $list)))

  (define (datum/annotation=? $a $b)
    (switch $a
      ((annotation? $annotation-a)
        (switch? $b
          ((annotation? $annotation-b)
            (annotation=? $annotation-a $annotation-b))))
      ((pair? $pair-a)
        (switch? $b
          ((pair? $pair-b)
            (and
              (datum/annotation=? (car $pair-a) (car $pair-b))
              (datum/annotation=? (cdr $pair-a) (cdr $pair-b))))))
      ; TODO: Check vector?, bytevector?, box?
      ((else $datum-a)
        (equal? $datum-a $b))))

  (define (annotation=? $a $b)
    (and
      (switch (annotation-expression $a)
        ((pair? $pair-a)
          (switch? (annotation-expression $b)
            ((pair? $pair-b)
              (and
                (datum/annotation=? (car $pair-a) (car $pair-b))
                (datum/annotation=? (cdr $pair-a) (cdr $pair-b))))))
        ; TODO: Check vector?, bytevector?, box?
        ((else $other-a)
          (equal? $other-a (annotation-expression $b))))
      (source-object=?
        (annotation-source $a)
        (annotation-source $b))
      (equal?
        (annotation-stripped $a)
        (annotation-stripped $b))))
)
