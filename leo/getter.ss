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

  (define allowed-word-general-categories '(Lu Ll Lt Lm Lo Sc Sm Sk So Pd Po))
  (define disallowed-word-chars '(#\" #\' #\` #\, #\# #\:))

  (define (char-word? $char)
    (and
      (member
        (char-general-category $char)
        allowed-word-general-categories)
      (not (member $char disallowed-word-chars))))

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
    (getter-lets
      ($char peek-char-getter)
      (cond
        ((char-word? $char) word-getter)
        ((char-numeric? $char) number-getter)
        ((char=? $char #\") string-literal-getter)
        (else (error-getter "unexpected char" $char)))))

  (define line-annotation-getter
    (getter-lets
      ($atom-annotation (annotation-getter atom-getter))
      (switch (annotation-stripped $atom-annotation)
        ((symbol? $symbol)
          (getter-lets
            ($char char-getter)
            (switch $char
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
              ((else _)
                (error-getter "unexpected char" $char)))))
        ((else $atom)
          (ending-getter
            (getter $atom-annotation)
            newline-getter)))))

  (define inline-annotation-getter
    (getter-lets
      ($atom-annotation (annotation-getter atom-getter))
      (switch (annotation-stripped $atom-annotation)
        ((symbol? $symbol)
          (getter-lets
            ($char peek-char/eof-getter)
            (switch $char
              ((eof? $eof)
                (getter $atom-annotation))
              ((char-space? _)
                (apply-getter append-annotation
                  (getter $atom-annotation)
                  (skip-char-getter inline-annotation-getter)))
              ((else _)
                (error-getter "unexpected char" $char)))))
        ((else $atom)
          (getter $atom-annotation)))))

  (define line-annotations-getter
    (list-getter (skip-newlines-getter (or-eof-getter line-annotation-getter))))

  (define inline-annotations-getter
    (apply-getter annotation-cons
      inline-annotation-getter
      (list-getter
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
