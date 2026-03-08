(library (leo get)
  (export
    word-annotation?-getter
    word?-getter
    atom?-getter)
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

  (define atom?-getter
    (getter-lets
      ($char/eof peek-char-getter)
      (lets
        ($atom?
          (and
            (not (eof-object? $char/eof))
            (or
              (char=? $char/eof #\")
              (char-numeric? $char/eof)
              (char-alphabetic? $char/eof))))
        (if $atom? datum-getter (getter #f)))))

  (define word-annotation?-getter
    (getter-lets
      ($word?-annotation (annotation-getter word?-getter))
      (and
        (annotation-stripped $word?-annotation)
        $word?-annotation)))
)
