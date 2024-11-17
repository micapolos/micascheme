(library (micalog verilog)
  (export
    program->code
    item->code
    size->code
    identifier->code
    value->code
    edge->code
    event->code)
  (import
    (except (micascheme) write)
    (code)
    (micalog keywords))

  (define (program->code $program)
    (syntax-case $program (circuit)
      ((circuit item ...)
        (apply code-append
          (intercalate
            (map item->code (syntaxes item ...))
            (code "\n"))))))

  (define (item->code $item)
    (syntax-case $item (register bit-count initial on write)
      (
        (register name
          (bit-count size)
          (initial initial-value)
          (on event)
          (write write-value))
        (code
          (newline-ended-code
            (colon-ended-code
              (space-separated-code
                "reg"
                (size->code #'size)
                (identifier->code #'name)
                "="
                (value->code #'initial-value))))
          (newline-ended-code
            (newline-separated-code
              (space-separated-code
                "always"
                (code
                  "@"
                  (code-in-round-brackets (event->code #'event)))
                "begin")
              (code-indent
                (colon-ended-code
                  (space-separated-code
                    (identifier->code #'name)
                    "<="
                    (value->code #'write-value))))
              "end"))))))

  (define (identifier->code $identifier)
    (fluent $identifier
      (syntax->datum)
      (symbol->string)
      (string->list)
      (map-using identifier-char)
      (list->string)
      (string-code)))

  (define (identifier-char $char)
    (case $char
      ((#\- #\?) #\_)
      (else $char)))

  (define (value->code $value)
    (syntax-case $value (+)
      (id (identifier? #'id)
        (identifier->code #'id))
      (number (number? (datum number))
        (number-code (datum number)))
      ((+ lhs rhs)
        (space-separated-code
          (value->code #'lhs)
          "+"
          (value->code #'rhs)))))

  (define (size->number $size)
    (switch (syntax->datum $size)
      ((positive-integer? $positive-integer)
        $positive-integer)
      ((else $other)
        (syntax-error $size "not positive integer"))))

  (define (size->code $size)
    (code-in-square-brackets
      (number-code (- (size->number $size) 1))
      ":"
      (number-code 0)))

  (define (event->code $event)
    (syntax-case $event ()
      ((edge value)
        (space-separated-code
          (edge->code #'edge)
          (value->code #'value)))))

  (define (edge->code $edge)
    (syntax-case $edge (positive-edge negative-edge)
      (positive-edge (code "posedge"))
      (negative-edge (code "negedge"))))
)
