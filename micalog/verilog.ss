(library (micalog verilog)
  (export
    program->code
    item->code
    size->code
    identifier->code
    value->code
    edge->code
    event->code
    check-verilog)
  (import
    (micascheme)
    (code)
    (prefix (micalog keywords) %))
  (export (import (rename (only (micascheme) lines-string) (lines-string lines))))

  (define (program->code $program)
    (syntax-case $program (%circuit)
      ((%circuit item ...)
        (apply code-append
          (intercalate
            (map item->code (syntaxes item ...))
            (code "\n"))))))

  (define (item->code $item)
    (syntax-case $item (%register %bit-count %initial %on %set %if %wire)
      (
        (%register name
          (%bit-count size)
          (%initial initial-value)
          (%on event)
          (%if if-value)
          (%set set-value))
        (code
          (newline-ended-code
            (colon-ended-code
              (code
                (space-separated-code
                  "reg"
                  (size->code #'size)
                  (identifier->code #'name))
                (if (datum initial-value)
                  (code " = " (value->code #'initial-value))
                  (code "")))))
          (newline-ended-code
            (newline-separated-code
              (space-separated-code
                "always"
                (code
                  "@"
                  (code-in-round-brackets (event->code #'event)))
                "begin")
              (code-indent
                (if (datum if-value)
                  (newline-separated-code
                    (space-separated-code
                      "if"
                      (code-in-round-brackets (value->code #'if-value))
                      "begin")
                    (code-indent
                      (set->code #`(%set name set-value)))
                    "end")
                  (set->code #`(%set name set-value))))
              "end"))))
      (
        (%wire name
          (%bit-count size)
          value)
        (code
          (newline-ended-code
            (colon-ended-code
              (space-separated-code
                "wire"
                (size->code #'size)
                (identifier->code #'name)
                "="
                (value->code #'value))))))))

  (define (identifier->code $identifier)
    (fluent $identifier
      (syntax->datum)
      (symbol->string)
      (string->list)
      (map-using identifier-char)
      (list->string)
      (string-code)))

  (define (set->code $set)
    (syntax-case $set (%set)
      ((%set id value)
        (colon-ended-code
          (space-separated-code
            (identifier->code (identifier id))
            "<="
            (value->code #'value))))))

  (define (identifier-char $char)
    (case $char
      ((#\- #\?) #\_)
      (else $char)))

  (define (value->code $value)
    (syntax-case $value (%+)
      (id (identifier? #'id)
        (identifier->code #'id))
      (number (number? (datum number))
        (number-code (datum number)))
      ((%+ lhs rhs)
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
    (syntax-case $edge (%positive-edge %negative-edge)
      (%positive-edge (code "posedge"))
      (%negative-edge (code "negedge"))))

  (define-case-syntax (check-verilog (id body) string)
    #`(check
      (equal?
        (code-string
          (#,(identifier-append #'id #'id #'->code) #'body))
        string)))
)
