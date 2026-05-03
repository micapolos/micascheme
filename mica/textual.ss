(library (mica textual)
  (export
    (rename (%textual textual))
    textual/value->string
    textual-null
    textual-char
    textual-char-passing
    textual-alphabetic-char
    textual-numeric-char
    check-textual)
  (import
    (scheme)
    (data)
    (syntaxes)
    (procedure)
    (lets)
    (check)
    (list)
    (prefix (mica reader) reader-of-)
    (only (mica reader) read-string)
    (mica writer))

  (data (textual reader writer))

  (define (textual/value->string $textual $value)
    (writer/value->string (textual-writer $textual) $value))

  (define-rules-syntaxes (keywords ok error)
    ((%textual ch)
      (char? (datum ch))
      (make-textual
        (reader-of-the ch)
        (writer ch)))
    ((%textual str)
      (string? (datum str))
      (make-textual
        (reader-of-the str)
        (writer str)))
    ((%textual t) t)
    ((first-of-textual)
      (make-textual
        (reader-of-one-of)
        (or-writer)))
    (textual-null
      (make-textual
        (reader-of-replace reader-of-eof null)
        null-writer))
    (textual-char
      (make-textual
        reader-of-char
        char-writer))
    ((textual-char-passing test?)
      (make-textual
        (reader-of-?char test?)
        (?char-writer test?)))
    (textual-alphabetic-char (textual-char-passing char-alphabetic?))
    (textual-numeric-char (textual-char-passing char-numeric?))
    ((check-textual-rule t (ok str))
      (check
        (equal?
          (textual/value->string (%textual t)
            (read-string (textual-reader (%textual t)) str))
          str)))
    ((check-textual-rule t (error str))
      (check
        (raises
          (read-string (textual-reader (%textual t))))))
    ((check-textual t rule ...)
      (lets
        ($t (%textual t))
        (run (check-textual-rule $t rule) ...))))
)
