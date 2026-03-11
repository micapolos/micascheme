(library (leo mica parser)
  (export
    identifier
    special-atom)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define lambda read open-input-string always default)
    (mica parser)
    (leo char))

  (define constituent-char
    (?char char-constituent?))

  (define special-initial-char
    (?char char-special-initial?))

  (define special-subsequent-char
    (?char char-special-subsequent?))

  (define digit-char
    (?char char-digit?))

  (define initial-string
    (one-of
      (string constituent-char)
      (string special-initial-char)
      ; inline-hex-escape-string
      ))

  (define subsequent-category-char
    (?char char-subsequent-category?))

  (define subsequent-string
    (one-of
      initial-string
      (string digit-char)
      (string subsequent-category-char)
      (string special-subsequent-char)))

  (define subsequent-list-string
    (list-string (list subsequent-string)))

  (define peculiar-identifier-string
    (one-of "+" "..."
      (string-append "-"
        (map
          (optional (string-append ">" subsequent-list-string))
          (default "")))))

  (define identifier-string
    (one-of
      (string-append initial-string subsequent-list-string)
      peculiar-identifier-string))

  (define identifier
    (map identifier-string
      (lambda ($string)
        (read
          (open-input-string $string)))))

  (define special-atom
    (prefixed #\#
      (one-of
        (map #\t (always #t))
        (map #\f (always #f))
        (map
          (prefixed #\\ identifier-string)
          (lambda ($string)
            (read
              (open-input-string
                (%string-append "#\\" $string))))))))
)
