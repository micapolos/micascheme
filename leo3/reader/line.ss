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
      (one-of
        (replace #\newline $identifier-annotation)
        (lets
          ($rhs-line-annotations
            (prefixed #\:
              (one-of
                (prefixed #\space (list line-annotation))
                (prefixed #\newline (indented line-annotations)))))
          (list-annotation
            (return
              (%cons $identifier-annotation $rhs-line-annotations)))))))

  (define line-annotation
    (one-of
      (suffixed (annotation literal) #\newline)
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
