(library (verilog code)
  (export
    module->code
    io->code
    parameter->code
    declaration->code
    declarations->code
    statement->code
    size->code
    name->code
    expr->code
    edge->code
    event->code
    check-verilog
    check-verilogs)
  (import
    (micascheme)
    (code)
    (prefix (verilog keywords) %))
  (export
    (import
      (rename
        (only (micascheme) lines-string)
        (lines-string lines))))

  (define (module->code $module)
    (syntax-case $module (%module)
      ((%module (name parameter ...) declaration ...)
        (code
          (space-separated-code
            "module"
            (name->code (identifier name))
            (code
              (code-in-round-brackets
                (code-in-newlines
                  (indented-code
                    (list->separated-code (code ",\n")
                      (map parameter->code (syntaxes parameter ...))))))
              ";\n"))
          (indented-code (declarations->code (syntaxes declaration ...)))
          (newline-ended-code "endmodule")))))

  (define (parameter->code $parameter)
    (syntax-case $parameter (%input %output)
      ((io name)
        (space-separated-code
          (io->code #'io)
          (name->code #'name)))
      ((io vector name)
        (space-separated-code
          (io->code #'io)
          (range-declaration->code #'vector)
          (name->code #'name)))))

  (define (io->code $io)
    (syntax-case $io (%input %output)
      (%input (code "input"))
      (%output (code "output"))))

  (define (declarations->code $declarations)
    (list->code (map declaration->code $declarations)))

  (define (declaration->code $item)
    (syntax-case $item (%always %assign)
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
        (for-all range->code? (syntaxes array ...))
        (declaration-components->code #'kind #f #'name (syntaxes array ...) #f))
      ((kind vector name array ...)
        (for-all range->code? (syntaxes vector array ...))
        (declaration-components->code #'kind #'vector #'name (syntaxes array ...) #f))
      ((kind name array ... expr)
        (for-all range->code? (syntaxes array ...))
        (declaration-components->code #'kind #f #'name (syntaxes array ...) #'expr))
      ((kind vector name array ... expr)
        (for-all range->code? (syntaxes vector array ...))
        (declaration-components->code #'kind #'vector #'name (syntaxes array ...) #'expr))))

  (define (statement->code $statement)
    (syntax-case $statement (%set! %assign %cond %else)
      ((%set! lhs rhs)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              (lhs->code #'lhs)
              "<="
              (expr->code #'rhs)))))
      ((%assign lhs rhs)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              "assign"
              (lhs->code #'lhs)
              "="
              (expr->code #'rhs)))))
      ((%cond clause clause* ... (%else else-body ...))
        (newline-ended-code
          (list->code
            (intercalate
              (append
                (list (cond-clause->code (syntax clause)))
                (map cond-clause->code (syntaxes clause* ...))
                (list (block->code (syntaxes else-body ...))))
              (code " else ")))))
      ((%cond clause clause* ...)
        (newline-ended-code
          (list->code
            (intercalate
              (cons
                (cond-clause->code (syntax clause))
                (map cond-clause->code (syntaxes clause* ...)))
              (code " else ")))))))

  (define (block->code $block)
    (syntax-case $block ()
      ((body ...)
        (code
          (newline-ended-code "begin")
          (fluent (syntaxes body ...)
            (map-using statement->code)
            (list->code)
            (code-indent))
          "end"))))

  (define (cond-clause->code $clause)
    (syntax-case $clause ()
      ((cond body ...)
        (space-separated-code
          "if"
          (code-in-round-brackets (expr->code #'cond))
          (block->code #'(body ...))))))

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
          (if (and (free-identifier=? $kind #'%wire) $expr)
            (syntax-error $expr "not valid for wire")
            (and $expr (space-separated-code "=" (expr->code $expr))))))))

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

  (define (expr->code $value)
    (syntax-case $value (%+ %- %and %or %inv %ref %if %append)
      (id (identifier? #'id)
        (name->code #'id))
      (integer (nonnegative-integer? (datum integer))
        (case (datum integer)
          ((0 1) (number-code (datum integer)))
          (else (string-code (string-append "'b" (number->string (datum integer) 2))))))
      (integer (integer? (datum integer))
        (number-code (datum integer) 2))
      ((%+ expr ...)
        (ops-default->code " + " (code 0) (syntaxes expr ...)))
      ((%- expr)
        (op->code "-" #'expr))
      ((%- expr expr* ...)
        (ops->code " - " (syntaxes expr expr* ...)))
      ((%and expr ...)
        (ops-default->code " & " (code "~0") (syntaxes expr ...)))
      ((%or expr ...)
        (ops-default->code " | " (code 0) (syntaxes expr ...)))
      ((%inv rhs)
        (op->code "~" #'rhs))
      ((%ref expr selector ...)
        (code
          (expr->code #'expr)
          (list->code (map selector->code (syntaxes selector ...)))))
      ((%if cond true false)
        (space-separated-code
          (expr->code #'cond)
          "?"
          (expr->code #'true)
          ":"
          (expr->code #'false)))
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
        (ops->code $op $exprs))))

  (define (ops->code $op $exprs)
    (fluent $exprs
      (map-using expr->code)
      (intercalate (string-code $op))
      (list->code)))

  (define (size->number $size)
    (switch (syntax->datum $size)
      ((positive-integer? $positive-integer)
        $positive-integer)
      ((else $other)
        (syntax-error $size "not positive integer"))))

  (define (size->code $size)
    (code-in-square-brackets
      (fluent $size (size->number) (- 1) (number-code))
      ":"
      (number-code 0)))

  (define (event->code $event)
    (syntax-case $event (%*)
      (%*
        (code "*"))
      ((edge value)
        (space-separated-code
          (edge->code #'edge)
          (expr->code #'value)))))

  (define (edge->code $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge (code "posedge"))
      (%negedge (code "negedge"))))

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
      (syntax-case $selector ()
        (range (range->code? #'range)
          (range->code #'range))
        ((index)
          (index->code #'index)))))

  (define (range->code? $range)
    (syntax-case? $range (%to)
      ((from %to to)
        (code
          (index->code #'from)
          ":"
          (index->code #'to)))))

  (define (range->code $range)
    (or
      (range->code? $range)
      (syntax-error $range "invalid range")))

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

  (define-case-syntax (check-verilogs (id body ...) string)
    #`(check
      (equal?
        (code-string
          (#,(identifier-append #'id #'id #'->code) (list #'body ...)))
        string)))
)
