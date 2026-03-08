(library (leo getter)
  (export
    indent?-getter

    atom-annotation/eof-getter
    line-annotation/eof-getter
    script-annotation-getter

    atom/eof-getter
    line/eof-getter
    script-getter)
  (import
    (micascheme)
    (getter))

  (define indent?-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof)
          (getter #f))
        ((char-space? _)
          (getter-map (exact-string-getter "  ") (lambda (_) #t)))
        ((else $other)
          (getter #f)))))

  (define atom-annotation/eof-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof)
          (getter $eof))
        ((char-whitespace? $char-whitespace)
          (throw atom-getter $char-whitespace))
        ((else _)
          (getter-lets
            ($datum-annotation/eof datum-annotation/eof-getter)
            (getter
              (switch $datum-annotation/eof
                ((eof? $eof)
                  (throw atom-getter $eof))
                ((else $datum-annotation)
                  (switch (annotation-stripped $datum-annotation)
                    ((number? $number)
                      $datum-annotation)
                    ((string? $string)
                      $datum-annotation)
                    ((symbol? $symbol)
                      $datum-annotation)
                    ((else $other)
                      (throw atom-getter $datum-annotation)))))))))))

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
                (getter $atom-annotation/eof))
              ((else _)
                (throw line-annotation/eof-getter $atom-annotation/eof)))))
        ((else _)
          (ending-getter
            (getter $atom-annotation/eof)
            newline-getter)))))

  (define script-annotation-getter
    (list-getter line-annotation/eof-getter))

  (define atom/eof-getter
    (apply-getter annotation/eof-stripped atom-annotation/eof-getter))

  (define line/eof-getter
    (apply-getter annotation/eof-stripped line-annotation/eof-getter))

  (define script-getter
    (getter-lets
      ($annotations script-annotation-getter)
      (getter (map annotation-stripped $annotations))))
)
