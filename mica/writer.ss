(library (mica writer)
  (export
    (rename (%writer writer))
    null-writer
    char->writer
    string->writer
    ?char-writer
    char-writer
    string-writer
    or-writer
    writer/value->string
    check-writer)
  (import
    (scheme)
    (lets)
    (switch)
    (boolean)
    (code)
    (check)
    (throw)
    (keyword)
    (procedure)
    (syntaxes))

  (define (writer/value->code-box? $writer $value)
    ($writer $value))

  (define (writer/value->code $writer $value)
    (switch (writer/value->code-box? $writer $value)
      ((box? $box) (unbox $box))
      ((else _) (throw writer/value->code $writer $value))))

  (define (writer/value->string $writer $value)
    (code-string (writer/value->code $writer $value)))

  (define null-writer
    (lambda ($value)
      (and
        (null? $value)
        (box (empty-code)))))

  (define (char->writer $char)
    (lambda ($value)
      (and
        (equal? $value $char)
        (box (char-code $char)))))

  (define (string->writer $string)
    (lambda ($value)
      (and
        (equal? $value $string)
        (box (string-code $string)))))

  (define char-writer
    (lambda ($value)
      (switch? $value
        ((char? $char)
          (box (char-code $char))))))

  (define string-writer
    (lambda ($value)
      (switch? $value
        ((string? $string)
          (box (string-code $string))))))

  (define (?char-writer $test?)
    (lambda ($value)
      (switch? $value
        ((char? $char)
          (and
            ($test? $char)
            (box (char-code $char)))))))

  (define alphabetic-char-writer (?char-writer char-alphabetic?))
  (define numeric-char-writer (?char-writer char-numeric?))

  (define-rules-syntaxes (keywords)
    ((%writer ch)
      (char? (datum ch))
      (char->writer ch))
    ((%writer str)
      (string? (datum str))
      (string->writer str))
    ((%writer x) x)
    ((or-writer)
      (lambda (_) #f))
    ((or-writer x xs ...)
      (lets
        ($writer (%writer x))
        (lambda ($value)
          (or
            (writer/value->code-box? $writer $value)
            (writer/value->code-box? (or-writer xs ...) $value)))))
    ((check-writer-rule wr (writes val str))
      (free-keyword? writes)
      (check (equal? (writer/value->string wr val) str)))
    ((check-writer-rule wr (raises val))
      (free-keyword? raises)
      (check (raises (writer/value->string wr val))))
    ((check-writer wr rule ...)
      (lets
        ($writer wr)
        (run (check-writer-rule $writer rule) ...))))
)
