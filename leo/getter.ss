(library (leo getter)
  (export
    word-getter
    number-getter
    string-literal-getter
    atom-getter

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
    (getter))

  (data*
    inline-style
    colon-style
    block-style
    (fragment style ref))

  (enum (style inline-style colon-style block-style))

  (define allowed-word-general-categories
    '(Lu Ll Lt Lm Lo Nd Sc Sm Sk So Pd Po))

  (define disallowed-word-chars
    '(#\" #\' #\` #\, #\# #\:))

  (define (word-char? $char)
    (and
      (member
        (char-general-category $char)
        allowed-word-general-categories)
      (not (member $char disallowed-word-chars))))

  (define (first-word-char? $char)
    (and
      (word-char? $char)
      (not (char-numeric? $char))))

  (define (first-number-char? $char)
    (or
      (char-numeric? $char)
      (char=? $char #\+)
      (char=? $char #\-)))

  (define (first-string-char? $char)
    (char=? $char #\"))

  (define (first-atom-char? $char)
    (or
      (first-word-char? $char)
      (first-number-char? $char)
      (first-string-char? $char)))

  (define word-getter
    (getter-lets
      ($string (string-while-getter word-char?))
      (if (string-empty? $string)
        (error-getter "empty" 'word)
        (getter (string->symbol $string)))))

  ; TODO: negative, positive, floating-point
  (define number-getter
    (getter-lets
      ($string (string-while-getter char-numeric?))
      (if (string-empty? $string)
        (error-getter "empty" 'number)
        (getter (string->number $string)))))

  ; TODO: escaping!!!
  (define string-literal-getter
    (getter-lets
      (_ (exact-char-getter #\"))
      ($string (string-while-getter (lambda ($char) (not (char=? $char #\")))))
      (_ (exact-char-getter #\"))
      (getter $string)))

  (define atom-getter
    (getter-switch peek-char-getter
      ((first-word-char? _) word-getter)
      ((first-number-char? _) number-getter)
      ((first-string-char? _) string-literal-getter)
      ((else $char) (error-getter "unexpected char" $char))))

  (define word-getter-item (getter-item first-word-char? word-getter))
  (define number-getter-item (getter-item first-number-char? number-getter))
  (define string-getter-item (getter-item first-string-char? string-literal-getter))
  (define atom-getter-item (getter-item first-atom-char? atom-getter))
  (define comma-separator-getter-item (getter-item char-comma? (exact-getter ", ")))

  (define atom-annotation-getter
    (annotation-getter atom-getter stripped-annotation))

  (define line-annotation-getter
    (annotation-getter
      (getter-lets
        ($atom-annotation atom-annotation-getter)
        (switch (annotation-stripped $atom-annotation)
          ((symbol? $symbol)
            (getter-lets
              (getter-switch rhs-line-annotations-getter
                ((null? _)
                  (getter $atom-annotation))
                ((else $rhs-line-annotations)
                  (getter (cons $atom-annotation $rhs-line-annotations))))))
          ((else _)
            (ending-getter
              (getter $atom-annotation)
              newline-getter))))
      (lambda ($line $source-object)
        (switch $line
          ((annotation? $line) $line)
          ((else $list) (list-annotation $list $source-object))))))

  (define rhs-line-annotations-getter
    (getter-switch char-getter
      ((char-space? _)
        (apply-getter list line-annotation-getter))
      ((char-colon? _)
        (ending-getter
          (starting-getter space-getter inline-annotations-getter)
          newline-getter))
      ((char-newline? _)
        (indented-getter line-annotations-getter))
      ((else $char)
        (error-getter "unexpected char" $char))))

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
    (list-getter (skip-newlines-getter line-annotation-getter)))

  (define inline-annotations-getter
    (non-empty-separated-getter
      inline-annotation-getter
      comma-separator-getter-item))

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
