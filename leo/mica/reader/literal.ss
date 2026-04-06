(library (leo mica reader literal)
  (export
    number
    string-literal
    special-literal
    literal)
  (import
    (prefix (scheme) %)
    (prefix (predicate) %)
    (prefix (char) %)
    (mica reader)
    (leo mica reader identifier))

  ; TODO: Implement it properly
  (%define number
    (string->datum
      (list->string
        (non-empty-list-of digit-char))))

  ; TODO: Implement it properly
  (%define string-literal
    (wrapped
      #\"
      (list-string (list-of (string (first-char (not #\" #\newline) char))))
      #\"))

  ; TODO: Implement it properly
  (%define special-literal
    (string->datum
      (string-append "#"
        (one-of
          (one-of "t" "f")
          (string-append "\\"
            (one-of
              (map (?char (%not? %char-alphabetic?)) %string)
              (map (cons #\x (non-empty-list-of (?char %char-hex?))) %list->string)
              identifier-string))))))

  (%define literal
    (one-of
      number
      identifier
      string-literal
      special-literal))
)
