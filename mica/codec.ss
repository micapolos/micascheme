(library (mica codec)
  (export
    codec-string
    codec-value
    char-codec
    string-codec
    ?char-codec
    check-codec)
  (import
    (scheme)
    (data)
    (syntaxes)
    (procedure)
    (lets)
    (check)
    (list)
    (keyword)
    (prefix (mica reader) reader-of-)
    (only (mica reader) read-string)
    (mica writer))

  (data (codec reader writer))

  (define (codec-string $codec $value)
    (writer-string (codec-writer $codec) $value))

  (define (codec-value $codec $string)
    (read-string (codec-reader $codec) $string))

  (define char-codec
    (make-codec reader-of-char char-writer))

  (define string-codec
    (make-codec reader-of-string string-writer))

  (define (?char-codec $predicate)
    (make-codec
      (reader-of-?char $predicate)
      (writer-filter $predicate char-writer)))

  (define-rules-syntaxes (keywords)
    ((check-codec c (works s1 ...) (raises s2 ...))
      (free-keywords? works raises)
      (lets
        ($codec c)
        (run
          (check
            (equal?
              (codec-string $codec (codec-value $codec s1))
              s1))
          ...
          (check
            (raises
              (codec-value $codec s2)))
          ...))))
)
