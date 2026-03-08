(library (leo get)
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
    (get))

  (define indent?-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof-object? $eof)
          (getter #f))
        (((partial char=? #\space) _)
          (getter-map (exact-string-getter "  ") (lambda (_) #t)))
        ((else $other)
          (getter #f)))))

  (define atom-annotation/eof-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof-object? $eof-object)
          (getter $eof-object))
        ((char-whitespace? $char-whitespace)
          (throw atom-getter $char-whitespace))
        ((else _)
          (getter-lets
            ($datum-annotation/eof datum-annotation/eof-getter)
            (getter
              (switch $datum-annotation/eof
                ((eof-object? $eof-object)
                  (throw atom-getter $eof-object))
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
        ((eof-object? $eof-object)
          (getter $eof-object))
        ((symbol? $symbol)
          (getter-lets
            ($char/eof char/eof-getter)
            (switch $char/eof
              ((eof-object? _)
                (getter $atom-annotation/eof))
              (((partial char=? #\space) _)
                (apply-getter append-annotation
                  (getter $atom-annotation/eof)
                  line-annotation/eof-getter))
              (((partial char=? #\newline) _)
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
