(library (leo get)
  (export
    word-annotation?-getter
    word?-getter
    atom-getter
    line-getter)
  (import
    (micascheme)
    (get))

  (define word?-getter
    (getter-lets
      ($string (test?-string-getter char-alphabetic?))
      (getter
        (and
          (not (string-empty? $string))
          (string->symbol $string)))))

  (define atom-getter
    (getter-lets
      ($char peek-char-getter)
      (if (char-whitespace? $char)
        (throw atom-getter $char)
        (getter-lets
          ($datum datum-getter)
          (getter
            (switch $datum
              ((number? $number) $number)
              ((string? $string) $string)
              ((symbol? $symbol) $symbol)
              ((else $other) (throw atom-getter $other))))))))

  (define line-getter
    (getter-lets
      ($atom atom-getter)
      (switch $atom
        ((symbol? $atom)
          (getter-lets
            ($eof? eof?-getter)
            (if $eof?
              (getter $atom)
              (getter-lets
                ($space (exact-char-getter #\space))
                ($line line-getter)
                (getter (list $atom $line))))))
        ((else $other)
          (getter-lets
            ($char? char?-getter)
            (switch $char?
              ((false? _) (getter $other))
              (((char=? $char? #\newline) _) (getter $other))
              ((else $other) (throw atom-getter $other))))))))


  (define word-annotation?-getter
    (getter-lets
      ($word?-annotation (annotation-getter word?-getter))
      (and
        (annotation-stripped $word?-annotation)
        $word?-annotation)))
)
