(library (leo getter)
  (export
    line-annotation-getter
    line-annotations-getter

    inline-annotation-getter
    inlines-annotation-getter

    line-getter
    lines-getter

    inline-getter
    inlines-getter)
  (import
    (micascheme)
    (getter)
    (prefix (mica reader) %)
    (prefix (leo mica reader identifier) %)
    (prefix (leo mica reader literal) %))

  (data*
    inline-style
    colon-style
    block-style
    (fragment style ref))

  (define comma-separator-getter-item
    (getter-item char-comma? (exact-getter ", ")))

  (define atom-annotation-getter
    (getter-item-getter (%annotation %literal)))

  (define line-annotation-getter
    (annotation-getter
      (getter-switch peek-char-getter
        ((char-colon? _)
          (skip-char-getter colon-line-annotations-getter))
        ((else _)
          (getter-lets
            ($atom-annotation atom-annotation-getter)
            (getter-switch rhs-line-annotations-getter
              ((null? _)
                (getter $atom-annotation))
              ((else $rhs-line-annotations)
                (getter (cons $atom-annotation $rhs-line-annotations)))))))
      (lambda ($line $source-object)
        (switch $line
          ((annotation? $line) $line)
          ((else $list) (list-annotation $list $source-object))))))

  (define rhs-line-annotations-getter
    (getter-switch char-getter
      ((char-space? _) space-line-annotations-getter)
      ((char-colon? _) colon-line-annotations-getter)
      ((char-comma? _) comma-line-annotations-getter)
      ((char-newline? _) newline-line-annotations-getter)
      ((else $char) (error-getter "unexpected char" $char))))

  (define inline-annotation-getter
    (getter-lets
      ($atom-annotation atom-annotation-getter)
      (switch (annotation-stripped $atom-annotation)
        ((symbol? $symbol)
          (getter-switch peek-char/eof-getter
            ((eof? _)
              (getter $atom-annotation))
            ((char-space? _)
              (apply-getter append-annotation
                (getter $atom-annotation)
                (skip-char-getter inline-annotation-getter)))
            ((else $unexpected-char)
              (getter $atom-annotation))))
        ((else _)
          (getter $atom-annotation)))))

  (define line-annotations-getter
    (reject?-accept?-list-getter
      char-newline?
      (getter-item-first-char? %literal)
      line-annotation-getter))

  (define inline-annotations-getter
    (non-empty-separated-getter
      inline-annotation-getter
      comma-separator-getter-item))

  (define space-line-annotations-getter
    (apply-getter list line-annotation-getter))

  (define colon-line-annotations-getter
    (getter-switch char-getter
      ((char-space? _)
        (ending-getter inline-annotations-getter newline-getter))
      ((char-newline? _)
        newline-line-annotations-getter)))

  (define comma-line-annotations-getter
    (replace-getter space-getter null))

  (define newline-line-annotations-getter
    (indented-getter line-annotations-getter))

  (define inlines-annotation-getter
    (annotation-getter inline-annotations-getter list-annotation))

  (define line-getter
    (apply-getter annotation-stripped line-annotation-getter))

  (define inline-getter
    (apply-getter annotation-stripped inline-annotation-getter))

  (define lines-getter
    (getter-lets
      ($annotations line-annotations-getter)
      (getter (map annotation-stripped $annotations))))

  (define inlines-getter
    (apply-getter annotation-stripped inlines-annotation-getter))
)
