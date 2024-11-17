(library (verilog code)
  (export
    program->code
    declaration->code
    declarations->code
    statement->code
    size->code
    name->code
    expr->code
    edge->code
    event->code
    check-verilog)
  (import
    (micascheme)
    (code)
    (prefix (verilog keywords) %))
  (export
    (import
      (rename
        (only (micascheme) lines-string)
        (lines-string lines))))

  (define (program->code $program)
    (syntax-case $program (%circuit)
      ((%circuit declaration ...)
        (declarations->code (syntaxes declaration ...)))))

  (define (declarations->code $declarations)
    (apply code-append
      (map declaration->code $declarations)))

  (define (declaration->code $item)
    (syntax-case $item (%always %:)
      ((%always event statement ...)
        (code
          (newline-ended-code
            (space-separated-code
              "always"
              (code "@"
                (code-in-round-brackets
                  (event->code #'event)))
              "begin"))
          (code-indent
            (list->code
              (map statement->code (syntaxes statement ...))))
          (newline-ended-code "end")))
      ((kind name array ...)
        (for-all range? (syntaxes array ...))
        (declaration-components->code #'kind #f #'name (syntaxes array ...) #f))
      ((kind vector name array ...)
        (for-all range? (syntaxes vector array ...))
        (declaration-components->code #'kind #'vector #'name (syntaxes array ...) #f))
      ((kind name array ... expr)
        (for-all range? (syntaxes array ...))
        (declaration-components->code #'kind #f #'name (syntaxes array ...) #'expr))
      ((kind vector name array ... expr)
        (for-all range? (syntaxes vector array ...))
        (declaration-components->code #'kind #'vector #'name (syntaxes array ...) #'expr))))

  (define (statement->code $statement)
    (syntax-case $statement (%set! %if)
      ((%set! lhs rhs)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              (lhs->code #'lhs)
              "<="
              (expr->code #'rhs)))))
      ((%if cond statement ...)
        (code
          (newline-ended-code
            (space-separated-code
              "if"
              (code-in-round-brackets (expr->code #'cond))
              "begin"))
          (code-indent
            (list->code
              (map statement->code (syntaxes statement ...))))
          (newline-ended-code "end")))))

  (define (declaration-kind->code $kind)
    (syntax-case $kind (%wire %reg)
      (%wire (code "wire"))
      (%reg (code "reg"))))

  (define (declaration-components->code $kind $vector $name $arrays $expr)
    (newline-ended-code
      (colon-ended-code
        (space-separated-code
          (declaration-kind->code $kind)
          (and $vector (range-declaration->code $vector))
          (name->code $name)
          (list->code (map range-declaration->code $arrays))
          (and $expr (space-separated-code "=" (expr->code $expr)))))))

  (define (name->code $name)
    (fluent $name
      (syntax->datum)
      (symbol->string)
      (string->list)
      (map-using name-char)
      (list->string)
      (string-code)))

  (define (name-char $char)
    (case $char
      ((#\- #\?) #\_)
      (else $char)))

  (define (name-set->code $name $value)
    (colon-ended-code
      (space-separated-code
        (name->code $name)
        "<="
        (expr->code $value))))

  (define (expr->code $value)
    (syntax-case $value (%+ %and %or %inv %ref %append)
      (id (identifier? #'id)
        (name->code #'id))
      (integer (integer? (datum integer))
        (number-code (datum integer)))
      ((%+ expr ...)
        (ops-default->code " + " (code 0) (syntaxes expr ...)))
      ((%and expr ...)
        (ops-default->code " & " (code "~0") (syntaxes expr ...)))
      ((%or expr ...)
        (ops-default->code " | " (code 0) (syntaxes expr ...)))
      ((%inv rhs)
        (op->code "~" #'rhs))
      ((%ref expr selector ...)
        (code
          (expr->code #'expr)
          (apply code-append
            (map selector->code (syntaxes selector ...)))))
      ((%append expr ...)
        (code-in-curly-brackets
          (ops->code ", " (syntaxes expr ...))))))

  (define (lhs->code $lhs)
    (syntax-case $lhs ()
      (id (identifier? #'id)
        (name->code #'id))
      ((id selector ...)
        (identifier? #'id)
        (code
          (name->code #'id)
          (list->code (map selector->code (syntaxes selector ...)))))))

  (define (op->code $op $rhs)
    (code
      (string-code $op)
      (expr->code $rhs)))

  (define (ops-default->code $op $default $exprs)
    (switch $exprs
      ((null? _) $default)
      ((else $exprs)
        (apply code-append
          (intercalate
            (map expr->code $exprs)
            (string-code $op))))))

  (define (ops->code $op $exprs)
    (apply code-append
      (intercalate
        (map expr->code $exprs)
        (string-code $op))))

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
          (expr->code #'value)))))

  (define (edge->code $edge)
    (syntax-case $edge (%positive-edge %negative-edge)
      (%positive-edge (code "posedge"))
      (%negative-edge (code "negedge"))))

  (define (type->vector-array-code $type)
    (syntax-case $type (%bit %vector)
      (%bit
        (values #f #f))
      ((%vector type size)
        (lets
          ((values $vector $array) (type->vector-array-code #'type))
          (if $vector
            (values $vector (code $array (size->code #'size)))
            (values (size->code #'size) #f))))))

  (define (selector->code $selector)
    (code-in-square-brackets
      (syntax-case $selector (%:)
        ((from %: to)
          (range->code $selector))
        ((index)
          (index->code #'index)))))

  (define (range? $range)
    (syntax-case $range (%:)
      ((_ %: _) #t)
      (_ #f)))

  (define (range->code $range)
    (syntax-case $range (%:)
      ((from %: to)
        (code
          (index->code #'from)
          ":"
          (index->code #'to)))))

  (define (range-declaration->code $range-declaration)
    (code-in-square-brackets (range->code $range-declaration)))

  (define (index->code $index)
    (switch (syntax->datum $index)
      ((nonnegative-integer? $integer)
        (number-code $integer))
      ((else _)
        (syntax-error $index "invalid index"))))

  (define-case-syntax (check-verilog (id body) string)
    #`(check
      (equal?
        (code-string
          (#,(identifier-append #'id #'id #'->code) #'body))
        string)))
)
