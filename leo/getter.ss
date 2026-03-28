(library (leo getter)
  (export
    quote-annotation?-getter
    unquote-annotation?-getter

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
    (symbol)
    (getter)
    (leo quotes)
    (prefix (mica reader) %)
    (prefix (leo mica reader identifier) %)
    (prefix (leo mica reader literal) %)
    (prefix (leo mica reader quotes) %))

  (data*
    inline-style
    colon-style
    block-style
    (fragment style ref))

  (define comma-separator-getter-item
    (getter-item char-comma? (exact-getter ", ")))

  (define atom-annotation-getter
    (getter-lets
      ($literal-annotation (getter-item-getter (%annotation %literal)))
      (switch (annotation-stripped $literal-annotation)
        ((symbol? $symbol)
          (getter
            (make-annotation
              $symbol
              (annotation-source $literal-annotation)
              $symbol)))
        ((else $other)
          (getter $literal-annotation)))))

  (define quote-annotation?-getter
    (getter-item-getter (%optional (%annotation %quote))))

  (define unquote-annotation?-getter
    (getter-item-getter (%optional (%annotation %unquote))))

  (define (quoted-annotation-getter $line-getter)
    (getter-switch quote-annotation?-getter
      ((annotation? $annotation)
        (getter-lets
          ($line (quoted-annotation-getter $line-getter))
          (annotation-getter
            (getter (list $annotation $line))
            list-annotation)))
      ((else _)
        $line-getter)))

  (define (unquoted-annotations-getter $line-annotations-getter)
    (getter-switch unquote-annotation?-getter
      ((annotation? $annotation)
        (getter-lets
          ($line-annotations (unquoted-annotations-getter $line-annotations-getter))
          ($quoted-annotations
            (annotation-getter
              (getter (cons $annotation $line-annotations))
              list-annotation))
          (getter (list $quoted-annotations))))
      ((else _)
        $line-annotations-getter)))

  (define (unquoted-annotation?-getter $line-annotation?-getter)
    (getter-switch unquote-annotation?-getter
      ((annotation? $annotation)
        (getter-switch (unquoted-annotation?-getter $line-annotation?-getter)
          ((annotation? $line-annotation)
            (annotation-getter
              (getter (list $annotation $line-annotation))
              list-annotation))
          ((else _)
            (error-getter '(unquoted nothing) 'unquote))))
      ((else _)
        $line-annotation?-getter)))

  (define line-annotation-getter
    (quoted-annotation-getter
      (getter-switch peek-char-getter
        ((char-colon? _)
          (annotation-getter
            (skip-char-getter colon-line-annotations-getter)
            list-annotation))
        ((else _)
          (getter-lets
            ($atom-annotation atom-annotation-getter)
            (getter-switch rhs-line-annotations-getter
              ((false? _)
                (getter $atom-annotation))
              ((else $rhs-line-annotations)
                (annotation-getter
                  (getter (cons $atom-annotation $rhs-line-annotations))
                  list-annotation))))))))

  (define rhs-line-annotations-getter
    (unquoted-annotations-getter
      (getter-switch char-getter
        ((char-space? _) space-line-annotations-getter)
        ((char-colon? _) colon-line-annotations-getter)
        ((char-comma? _) (non-null-getter comma-line-annotations-getter))
        ((char-newline? _) (non-null-getter newline-line-annotations-getter))
        ((else $char) (error-getter
          '(unexpected char)
          $char
          `(expected (one-of #\space #\: #\, #\newline)))))))

  (define (non-null-getter $list-getter)
    (getter-switch $list-getter
      ((null? _) (getter #f))
      ((else $list) (getter $list))))

  (define inline-annotation-getter
    (quoted-annotation-getter
      (getter-lets
        ($atom-annotation atom-annotation-getter)
        (switch (annotation-stripped $atom-annotation)
          ((symbol? $symbol)
            (getter-switch rhs-inline-annotation?-getter
              ((annotation? $rhs-annotation)
                (apply-getter append-annotation
                  (getter $atom-annotation)
                  (getter $rhs-annotation)))
              ((else _)
                (getter $atom-annotation))))
          ((else _)
            (getter $atom-annotation))))))

  (define rhs-inline-annotation?-getter
    (unquoted-annotation?-getter
      (getter-switch peek-char/eof-getter
        ((eof? _)
          (getter #f))
        ((char-space? _)
          (skip-char-getter inline-annotation-getter))
        ((else _)
          (getter #f)))))

  (define line-annotations-getter
    (reject?-accept?-list-getter
      char-newline?
      (lambda ($char)
        (or
          ((getter-item-first-char? %literal) $char)
          (char->quote? $char)))
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
