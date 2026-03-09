(import (scheme) (check) (annotation))

(define source-object-a (make-source-object (source-file-descriptor "a" 10) 20 30))
(define source-object-b (make-source-object (source-file-descriptor "b" 10) 20 30))

(check (annotation? (datum/annotation 123)))

(check
  (equal?
    (annotation-stripped (datum/annotation (foo bar)))
    '(foo bar)))

(check
  (annotation?
    (append-annotation
      (datum/annotation 10)
      (datum/annotation 20)
      (datum/annotation 30))))

(check
  (annotation=?
    (make-annotation "foo" source-object-a "foo")
    (make-annotation "foo" source-object-a "foo")))

(check
  (not
    (annotation=?
      (make-annotation "foo" source-object-a "foo")
      (make-annotation "bar" source-object-a "foo"))))

(check
  (not
    (annotation=?
      (make-annotation "foo" source-object-a "foo")
      (make-annotation "foo" source-object-b "foo"))))

(check
  (not
    (annotation=?
      (make-annotation "foo" source-object-a "foo")
      (make-annotation "foo" source-object-b "bar"))))

(check
  (annotation=?
    (make-annotation
      (list
        (make-annotation "foo" source-object-a "foo")
        (make-annotation "bar" source-object-a "bar"))
      source-object-a
      (list "foo" "bar"))
    (make-annotation
      (list
        (make-annotation "foo" source-object-a "foo")
        (make-annotation "bar" source-object-a "bar"))
      source-object-a
      (list "foo" "bar"))))

(check
  (not
    (annotation=?
      (make-annotation
        (list
          (make-annotation "foo" source-object-a "foo")
          (make-annotation "bar" source-object-a "bar"))
        source-object-a
        (list "foo" "bar"))
      (make-annotation
        (list
          (make-annotation "foo" source-object-a "foo")
          (make-annotation "zar" source-object-a "zar"))
        source-object-a
        (list "foo" "bar")))))
