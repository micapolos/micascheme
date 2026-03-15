(library (leo mica reader line)
  (export
    line
    lines

    line-annotation
    line-annotations)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader)
    (leo mica reader literal))

  (define line-annotation
    (one-of
      colon-line-annotation
      literal-line-annotation))

  (define literal-line-annotation
    (lets
      ($literal-annotation (annotation literal))
      ($rhs-line-annotations rhs-line-annotations)
      (%switch (%datum/annotation-stripped $rhs-line-annotations)
        ((%null? _)
          (return $literal-annotation))
        ((%else _)
          (list-annotation
            (return
              (%cons $literal-annotation $rhs-line-annotations)))))))

  (define inline-annotation
    (lets
      ($literal-annotation (annotation literal))
      (%switch (%annotation-stripped $literal-annotation)
        ((%symbol? $symbol)
          (switch (optional char)
            ((%false? _)
              (return $literal-annotation))
            ((%char-space? _)
              (apply
                (%append-annotation
                  (return $literal-annotation)
                  inline-annotation)))
            ((else _)
              (return $literal-annotation))))
        ((%else _)
          (return $literal-annotation)))))

  (define inline-annotations
    (non-empty-separated ", " inline-annotation))

  (define rhs-line-annotations
    (one-of
      (prefixed #\space rhs-space-line-annotations)
      (prefixed #\: rhs-colon-line-annotations)
      (prefixed #\, rhs-comma-line-annotations)
      (prefixed #\newline rhs-newline-line-annotations)))

  (define line-annotations
    (reject?-list %char-newline? line-annotation))

  (define rhs-space-line-annotations
    (apply (%list line-annotation)))

  (define rhs-colon-line-annotations
    (switch char
      ((%char-space? _) line-annotations)
      ((%char-newline? _) rhs-newline-line-annotations)
      ((else $char) (error "unexpected char" $char))))

  (define colon-line-annotation
    (list-annotation (prefixed #\: rhs-colon-line-annotations)))

  (define rhs-comma-line-annotations
    (replace #\space %null))

  (define rhs-newline-line-annotations
    (indented line-annotations))

  (define line
    (apply (%annotation-stripped line-annotation)))

  (define lines
    (lets
      ($annotations line-annotations)
      (return (%map %annotation-stripped $annotations))))
)
