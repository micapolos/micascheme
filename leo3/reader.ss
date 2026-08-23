(library
  (leo3 reader)
  (export
    word
    identifier)
  (import
    (only (scheme) define quote)
    (prefix (micascheme) %)
    (mica reader))

  (define word
    (map (non-empty-list-of alphabetic-char) %list->string))

  (define identifier
    (map
      (non-empty-separated (char space) word)
      (%lambda ($words)
        (%fold-left
          (%lambda ($folded $word)
            (cond
              ((string=? $word "to")
                (%cons "->" $folded))
              (else
                (%cons $word (%cons "-" $folded)))))
          '()
          $words

)
