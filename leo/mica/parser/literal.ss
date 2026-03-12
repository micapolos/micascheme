(library (leo mica parser literal)
  (export
    number
    string-literal
    special-literal
    literal)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica parser)
    (leo mica parser identifier))

  ; TODO: Implement it properly
  (define number
    (string->datum
      (list->string
        (non-empty-list digit-char))))

  ; TODO: Implement it properly
  (define string-literal
    (wrapped
      #\"
      (list-string (list (string (first-char (not #\" #\newline) char))))
      #\"))

  ; TODO: Implement it properly
  (define special-literal
    (string->datum
      (string-append "#"
        (one-of
          (one-of "t" "f")
          (string-append "\\"
            (one-of
              (string digit-char)
              identifier-string))))))

  (define literal
    (one-of
      number
      identifier
      string-literal
      special-literal))
)
