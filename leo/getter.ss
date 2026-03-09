(library (leo getter)
  (export
    word-getter
    number-getter
    string-literal-getter
    atom-getter

    line-annotation-getter

    line-getter
    lines-getter)
  (import
    (micascheme)
    (except (getter) line-getter))

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
                (error-getter "expected space or newline")))))
        ((else _)
          (ending-getter
            (getter $atom-annotation)
            newline-getter)))))

  (define line-annotations-getter
    (list-getter (or-eof-getter line-annotation-getter)))

  (define line-getter
    (apply-getter annotation-stripped line-annotation-getter))

  (define lines-getter
    (getter-lets
      ($annotations line-annotations-getter)
      (getter (map annotation-stripped $annotations))))
)
