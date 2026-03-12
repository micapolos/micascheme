(library (leo mica parser literal)
  (export literal number)
  (import
    (prefix (micascheme) %)
    (mica parser)
    (leo mica parser identifier))

  (%define number
    (string->datum
      (list->string
        (non-empty-list digit-char))))

  (%define literal
    (string->datum
      (string-append "#"
        (one-of
          (one-of "t" "f")
          (string-append "\\"
            (one-of
              (string digit-char)
              identifier-string))))))
)
