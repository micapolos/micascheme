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
    (prefixed #\#
      (one-of
        (map #\t (%always #t))
        (map #\f (%always #f))
        (map
          (prefixed #\\ (one-of (string digit-char) identifier-string))
          (%lambda ($string)
            (%read
              (%open-input-string
                (%string-append "#\\" $string))))))))
)
