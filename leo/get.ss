(library (leo get)
  (export
    word-annotation?-getter
    word?-getter)
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

  (define word-annotation?-getter
    (getter-lets
      ($word?-annotation (annotation-getter word?-getter))
      (and
        (annotation-stripped $word?-annotation)
        $word?-annotation)))
)
