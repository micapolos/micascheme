(library (mica writer)
  (export
    (rename (%writer writer))
    null-writer
    char->writer
    string->writer
    char-writer
    string-writer
    number-writer
    writer-filter
    writer-map
    or-writer
    writer-string
    check-writer)
  (import
    (scheme)
    (lets)
    (data)
    (switch)
    (boolean)
    (predicate)
    (code)
    (check)
    (throw)
    (keyword)
    (procedure)
    (syntaxes))

  (data (writer predicate code-procedure))

  (define (writer-test? $writer $value)
    ((writer-predicate $writer) $value))

  (define (writer-code $writer $value)
    ((writer-code-procedure $writer) $value))

  (define (writer-string $writer $value)
    (code-string (writer-code $writer $value)))

  (define null-writer
    (make-writer null? (always (empty-code))))

  (define char-writer
    (make-writer char? char-code))

  (define string-writer
    (make-writer string? string-code))

  (define (char->writer $char)
    (make-writer
      (partial equal? $char)
      (always (char-code $char))))

  (define (string->writer $string)
    (make-writer
      (partial equal? $string)
      (always (string-code $string))))

  (define number-writer
    (make-writer number?
      (dot string-code number->string)))

  (define (writer-filter $predicate $writer)
    (make-writer
      (and? (writer-predicate $writer) $predicate)
      (writer-code-procedure $writer)))

  (define (writer-map $predicate $transform $writer)
    (make-writer
      $predicate
      (lambda ($value)
        (writer-code $writer ($transform $value)))))

  (define-rules-syntaxes (keywords)
    ((%writer ch)
      (char? (datum ch))
      (char->writer ch))
    ((%writer str)
      (string? (datum str))
      (string->writer str))
    ((or-writer)
      (make-writer
        (always #f)
        (always (empty-code))))
    ((or-writer x) x)
    ((or-writer x y xs ...)
      (lets
        ($x x)
        ($y y)
        ($x-predicate (writer-predicate $x))
        ($y-predicate (writer-predicate $y))
        ($x-code (writer-code-procedure $x))
        ($y-code (writer-code-procedure $y))
        (or-writer
          (make-writer
            (or? $x-predicate $y-predicate)
            (lambda ($value)
              (if ($x-predicate $value) ($x-code $value) ($y-code $value))))
          xs ...)))
    ((check-writer-rule wr (writes val str))
      (free-keyword? writes)
      (check (equal? (writer-string wr val) str)))
    ((check-writer-rule wr (rejects val ...))
      (free-keyword? rejects)
      (begin (check (not (writer-test? wr val))) ...))
    ((check-writer wr rule ...)
      (lets
        ($writer wr)
        (run (check-writer-rule $writer rule) ...))))
)
