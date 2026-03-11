(library (leo mica parser literal)
  (export literal)
  (import
    (rename
      (only (micascheme) string-append always define lambda read open-input-string)
      (string-append %string-append))
    (mica parser)
    (leo mica parser identifier))

  (define literal
    (prefixed #\#
      (one-of
        (map #\t (always #t))
        (map #\f (always #f))
        (map
          (prefixed #\\ (one-of (string digit-char) identifier-string))
          (lambda ($string)
            (read
              (open-input-string
                (%string-append "#\\" $string))))))))
)
