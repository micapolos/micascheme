(library (leo3 reader identifier)
  (export
    letter-char
    word-string
    identifier)
  (import
    (prefix (micascheme) %)
    (prefix (leo3 symbolizer) %)
    (mica reader))

  (%define letter-char
    (one-of
      (range-char #\a #\z)))

  (%define digit-char
    (one-of
      (range-char #\0 #\9)))

  (%define letter-or-digit-char
    (one-of
      letter-char
      digit-char))

  (%define word-string
    (list-string
      (cons
        (string letter-char)
        (list-of (string letter-or-digit-char)))))

  (%define identifier
    (map
      (non-empty-separated " " word-string)
      (%lambda ($strings)
        (%symbolize (%map %string->symbol $strings)))))
)
