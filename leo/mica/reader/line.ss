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
    (lets
      ($literal-annotation (annotation literal))
      (%switch (%annotation-stripped $literal-annotation)
        ((%symbol? _)
          (switch rhs-line-annotations
            ((%null? _)
              (return $literal-annotation))
            ((else $rhs-line-annotations)
              (list-annotation (return (%cons $literal-annotation $rhs-line-annotations))))))
        ((%else _)
          (suffixed (return $literal-annotation) #\newline)))))

  (define rhs-line-annotations
    (switch char
      ((%char-space? _) space-line-annotations)
      ((%char-colon? _) colon-line-annotations)
      ((%char-comma? _) comma-line-annotations)
      ((%char-newline? _) newline-line-annotations)
      ((else $char) (error "unexpected char" $char))))

  (define line-annotations
    (list line-annotation))

  (define space-line-annotations
    (apply (%list line-annotation)))

  (define colon-line-annotations
    (switch char
      ;((%char-space? _) (suffixed inline-annotations #\newline))
      ((%char-newline? _) newline-line-annotations)
      ((else $char) (error "unexpected char" $char))))

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
