(library (leo getter)
  (export
    word-getter
    number-getter
    string-literal-getter
    atom-getter

    line-annotation-getter
    line-annotations-getter

    inline-annotation-getter

    line-getter
    lines-getter

    inline-getter
    inlines-getter)
  (import
    (micascheme)
    (getter))

  (define allowed-word-general-categories
    '(Lu Ll Lt Lm Lo Sc Sm Sk So Pd Po))

  (define disallowed-word-chars
    '(#\" #\' #\` #\, #\# #\:))

  (define (char-word? $char)
    (and
      (member
        (char-general-category $char)
        allowed-word-general-categories)
      (not (member $char disallowed-word-chars))))

  (define (char-word-selector? $char)
    (char-word? $char))

  (define (char-number-selector? $char)
    (or
      (char-numeric? $char)
      (char=? $char #\+)
      (char=? $char #\-)))

  (define (char-string-selector? $char)
    (char=? $char #\"))

  (define (char-atom-selector? $char)
    (or
      (char-word-selector? $char)
      (char-number-selector? $char)
      (char-string-selector? $char)))

  ; TODO: maybe allow non-alphabetic characters inside?
  (define word-getter
    (getter-lets
      ($string (string-while-getter char-word?))
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
      ((char-word-selector? _) word-getter)
      ((char-number-selector? _) number-getter)
      ((char-string-selector? _) string-literal-getter)
      ((else $char) (error-getter "unexpected char" $char))))

  (define atom-annotation-getter
    (annotation-getter atom-getter stripped-annotation))

  (define line-annotation-getter
    (getter-lets
      ($atom-annotation atom-annotation-getter)
      (switch (annotation-stripped $atom-annotation)
        ((symbol? $symbol)
          (getter-switch char-getter
            ((char-space? _)
              (apply-getter append-annotation
                (getter $atom-annotation)
                line-annotation-getter))
            ((char-newline? _)
              (getter-lets
                ($line-annotations (indented-getter line-annotations-getter))
                (switch $line-annotations
                  ((null? _)
                    (getter $atom-annotation))
                  ((else _)
                    (getter
                      (apply append-annotation
                        $atom-annotation
                        $line-annotations))))))
            ((else $unexpected-char)
              (error-getter "unexpected char" $unexpected-char))))
        ((else _)
          (ending-getter
            (getter $atom-annotation)
            newline-getter)))))

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
              (error-getter "unexpected char" $unexpected-char))))
        ((else _)
          (getter $atom-annotation)))))

  (define line-annotations-getter
    (list-getter (skip-newlines-getter line-annotation-getter)))

  (define inline-annotations-getter
    (apply-getter annotation-cons
      inline-annotation-getter
      (eol?-list-getter char-newline?
        (or-eof-getter
          (starting-getter
            (exact-string-getter ", ")
            inline-annotation-getter)))))

  (define line-getter
    (apply-getter annotation-stripped line-annotation-getter))

  (define inline-getter
    (apply-getter annotation-stripped inline-annotation-getter))

  (define lines-getter
    (getter-lets
      ($annotations line-annotations-getter)
      (getter (map annotation-stripped $annotations))))

  (define inlines-getter
    (getter-lets
      ($annotations inline-annotations-getter)
      (getter (map annotation-stripped $annotations))))
)
