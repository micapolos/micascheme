(library (leo3 reader identifier)
  (export
    letter-char
    word-string
    identifier)
  (import
    (prefix (micascheme) %)
    (mica reader))

  (%define letter-char
    (one-of
      (range-char #\a #\z)))

  (%define word-string
    (list-string (non-empty-list-of (string letter-char))))

  (%define identifier
    (map
      (non-empty-separated " " word-string)
      (%lambda ($strings)
        (%string->symbol
          (%apply %string-append
            (%intercalate $strings "-"))))))
)
