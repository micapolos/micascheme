(library (leo3 reader literal)
  (export
    number-literal
    string-literal
    literal)
  (import
    (prefix (scheme) %)
    (prefix (predicate) %)
    (prefix (char) %)
    (mica reader)
    (leo3 reader identifier))

  (%define number-literal
    (string->datum
      (list->string
        (non-empty-list-of (range-char #\0 #\9)))))

  (%define string-literal
    (wrapped
      #\"
      (list-string (list-of (string (first-char (not #\" #\newline) char))))
      #\"))

  (%define literal
    (one-of
      number-literal
      string-literal
      identifier))
)
