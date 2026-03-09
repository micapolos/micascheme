(library (leo getter)
  (export
    atom-getter

    atom-annotation/eof-getter
    line-annotation/eof-getter
    line-annotations-getter

    atom/eof-getter
    line/eof-getter
    lines-getter)
  (import
    (micascheme)
    (getter))

  ; TODO: maybe allow non-alphabetic characters inside?
  (define word-getter
    (getter-lets
      ($string (string-while-getter char-alphabetic?))
      (if (string-empty? $string)
        (error-getter "empty word")
        (getter (string->symbol $string)))))

  ; TODO: negative, positive, floating-point
  (define number-getter
    (getter-lets
      ($string (string-while-getter char-numeric?))
      (if (string-empty? $string)
        (error-getter "empty word")
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
        ((char-alphabetic? $char) word-getter)
        ((char-numeric? $char) number-getter)
        ((char=? $char #\") string-literal-getter)
        (else (error-getter "invalid char")))))

  (define atom-annotation/eof-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (getter $eof))
        ((else $atom) (annotation-getter atom-getter)))))

  (define line-annotation/eof-getter
    (getter-lets
      ($atom-annotation/eof atom-annotation/eof-getter)
      (switch (annotation/eof-stripped $atom-annotation/eof)
        ((eof? $eof)
          (getter $eof))
        ((symbol? $symbol)
          (getter-lets
            ($char/eof char/eof-getter)
            (switch $char/eof
              ((eof? _)
                (getter $atom-annotation/eof))
              ((char-space? _)
                (apply-getter append-annotation
                  (getter $atom-annotation/eof)
                  line-annotation/eof-getter))
              ((char-newline? _)
                (getter-lets
                  ($line-annotations (indented-getter line-annotations-getter))
                  (switch $line-annotations
                    ((null? _)
                      (getter $atom-annotation/eof))
                    ((else _)
                      (getter
                        (apply append-annotation
                          $atom-annotation/eof
                          $line-annotations))))))
              ((else _)
                (throw line-annotation/eof-getter $atom-annotation/eof)))))
        ((else _)
          (ending-getter
            (getter $atom-annotation/eof)
            newline-getter)))))

  (define line-annotations-getter
    (list-getter line-annotation/eof-getter))

  (define atom/eof-getter
    (apply-getter annotation/eof-stripped atom-annotation/eof-getter))

  (define line/eof-getter
    (apply-getter annotation/eof-stripped line-annotation/eof-getter))

  (define lines-getter
    (getter-lets
      ($annotations line-annotations-getter)
      (getter (map annotation-stripped $annotations))))
)
