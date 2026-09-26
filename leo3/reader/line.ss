(library (leo3 reader line)
  (export
    line
    lines

    line-annotation
    line-annotations)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader)
    (leo3 reader literal)
    (leo3 reader identifier))

  (define sentence-annotation
    (lets
      ($identifier-annotation (annotation identifier))
      ($rhs-line-annotations-opt rhs-line-annotations-opt)
      (%switch (%datum/annotation-stripped $rhs-line-annotations-opt)
        ((%null? _)
          (return $identifier-annotation))
        ((%else _)
          (list-annotation
            (return
              (%cons $identifier-annotation $rhs-line-annotations-opt)))))))

  (define line-annotation
    (one-of
      (annotation literal)
      sentence-annotation))

  (define line-annotations
    (reject?-list-of %char-newline? line-annotation))

  (define rhs-line-annotations-opt
    (one-of
      (prefixed #\:
        (one-of
          (prefixed #\space (list line-annotation))
          (prefixed #\newline (indented line-annotations))))
      (return %null)))

  (define line
    (apply (%annotation-stripped line-annotation)))

  (define lines
    (lets
      ($annotations line-annotations)
      (return (%map %annotation-stripped $annotations))))
)
