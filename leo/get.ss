(library (leo get)
  (export word?-getter)
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
)
