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
      (%switch (%annotation-stripped $literal-annotation)
        ((%symbol? _)
          (lets
            ($rhs-line-annotations rhs-line-annotations)
            (%switch (%datum/annotation-stripped $rhs-line-annotations)
              ((%null? _)
                (return $literal-annotation))
              ((%else _)
                (list-annotation
                  (return
                    (%cons $literal-annotation $rhs-line-annotations)))))))
        ((%else _)
          (suffixed (return $literal-annotation) #\newline)))))

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
    (switch char
      ((%char-space? _) space-line-annotations)
      ((%char-colon? _) colon-line-annotations)
      ((%char-comma? _) comma-line-annotations)
      ((%char-newline? _) newline-line-annotations)
      ((else $char) (error "unexpected char" $char))))

  (define line-annotations
    (reject?-list %char-newline? line-annotation))

  (define space-line-annotations
    (apply (%list line-annotation)))

  (define colon-line-annotations
    (switch char
      ((%char-space? _) (suffixed inline-annotations #\newline))
      ((%char-newline? _) newline-line-annotations)
      ((else $char) (error "unexpected char" $char))))

  (define colon-line-annotation
    (list-annotation (prefixed #\: colon-line-annotations)))

  (define comma-line-annotations
    (replace #\space %null))

  (define newline-line-annotations
    (indented line-annotations))

  (define line
    (apply (%annotation-stripped line-annotation)))

  (define lines
    (lets
      ($annotations line-annotations)
      (return (%map %annotation-stripped $annotations))))
)
