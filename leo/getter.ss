(library (leo getter)
  (export
    atom-annotation/eof-getter
    line-annotation/eof-getter
    line-annotations-getter

    atom/eof-getter
    line/eof-getter
    lines-getter)
  (import
    (micascheme)
    (getter))

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
