(library (leo get)
  (export
    word-annotation?-getter
    word?-getter)
  (import
    (micascheme)
    (get))

  (define word-annotation?-getter
    (getter-lets
      ($bfp bfp-getter)
      ($string (test?-string-getter char-alphabetic?))
      ($efp bfp-getter)
      ($sfd sfd-getter)
      (getter
        (and
          (not (string-empty? $string))
          (make-annotation
            (string->symbol $string)
            (make-source-object $sfd $bfp $efp)
            (string->symbol $string))))))

  (define word?-getter
    (getter-lets
      ($word-annotation? word-annotation?-getter)
      (getter
        (and $word-annotation?
          (annotation-stripped $word-annotation?)))))

)
