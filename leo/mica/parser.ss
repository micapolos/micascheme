(library (leo mica parser)
  (export
    identifier-string
    special-atom)
  (import
    (prefix (micascheme) %)
    (mica parser)
    (leo char))

  (%define constituent-string
    (map (?char char-constituent?) %string))

  (%define special-initial-string
    (map (?char char-special-initial?) %string))

  (%define special-subsequent-string
    (map (?char char-special-subsequent?) %string))

  (%define digit-string
    (map (?char char-digit?) %string))

  (%define initial-string
    (one-of
      constituent-string
      special-initial-string
      ; inline-hex-escape-string
      ))

  (%define subsequent-category-string
    (map (?char char-subsequent-category?) %string))

  (%define subsequent-char-string
    (one-of
      initial-string
      digit-string
      subsequent-category-string
      special-subsequent-string))

  (%define subsequent-string
    (string-list->string (list subsequent-char-string)))

  (%define peculiar-identifier-string
    (one-of
      "+"
      "..."
      (apply
        (%string-append
          "-"
          (map
            (optional (apply (%string-append ">" subsequent-string)))
            (%default ""))))))

  (%define identifier-string
    (one-of
      (apply
        (%string-append
          initial-string
          subsequent-string))
      peculiar-identifier-string))

  (%define special-atom
    (prefixed #\#
      (one-of
        (map #\t (%always #t))
        (map #\f (%always #f))
        (map
          (prefixed #\\ identifier-string)
          (%lambda ($string)
            (%read
              (%open-input-string
                (%string-append "#\\" $string))))))))
)
