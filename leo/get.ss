(library (leo get)
  (export
    atom-annotation/eof-getter
    line-annotation/eof-getter
    atom/eof-getter
    line/eof-getter)
  (import
    (micascheme)
    (get))

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
        ((symbol? $atom)
          (getter-lets
            ($eof? eof?-getter)
            (if $eof?
              (getter $atom)
              (getter-lets
                ($space (exact-char-getter #\space))
                ($line-annotation/eof line-annotation/eof-getter)
                (getter (append-annotation $atom-annotation/eof $line-annotation/eof))))))
        ((else $other)
          (getter-lets
            ($char/eof char/eof-getter)
            (switch $char/eof
              ((eof-object? _) (getter $other))
              (((partial char=? #\newline) _) (getter $other))
              ((else $other) (throw atom-getter $other))))))))

  (define atom/eof-getter
    (apply-getter annotation/eof-stripped atom-annotation/eof-getter))

  (define line/eof-getter
    (apply-getter annotation/eof-stripped line-annotation/eof-getter))
)
