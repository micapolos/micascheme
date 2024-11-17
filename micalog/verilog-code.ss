(library (micalog verilog-code)
  (export
    program->code
    declaration->code
    declarations->code
    size->code
    name->code
    expr->code
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
      ((%circuit declaration ...)
        (declarations->code (syntaxes declaration ...)))))

  (define (declarations->code $declarations)
    (apply code-append
      (map declaration->code $declarations)))

  (define (declaration->code $item)
    (syntax-case $item (%initial %on)
      ((name type (%initial expr) (%on action ...))
        (code
          (reg?-name-type-expr->code #t #'name #'type #'expr)
          (name-action->code #'name #'(%on action ...))))
      ((name type (%on action ...))
        (code
          (reg?-name-type-expr->code #t #'name #'type #f)
          (name-action->code #'name #'(%on action ...))))
      ((name type expr)
        (reg?-name-type-expr->code #f #'name #'type #'expr))
      ((name type)
        (code
          (reg?-name-type-expr->code #F #'name #'type #f)))))

  (define (reg?-name-type-expr->code $reg? $name $type $expr)
    (lets
      ((values $vector-code $array-code)
        (type->vector-array-code $type))
      (newline-ended-code
        (colon-ended-code
          (space-separated-code
            (string-code (if $reg? "reg" "wire"))
            $vector-code
            (name->code $name)
            $array-code
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

  (define (name-set->code $name $value)
    (colon-ended-code
      (space-separated-code
        (name->code $name)
        "<="
        (expr->code $value))))

  (define (expr->code $value)
    (syntax-case $value (%+ %and %or %not %ref %append)
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
      ((%not rhs)
        (op->code "~" #'rhs))
      ((%ref expr selector ...)
        (code
          (expr->code #'expr)
          (apply code-append
            (map selector->code (syntaxes selector ...)))))
      ((%append expr ...)
        (code-in-curly-brackets
          (ops->code ", " (syntaxes expr ...))))))

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

  (define (name-action->code $name $action)
    (syntax-case $action (%on)
      ((%on event body)
        (newline-ended-code
          (newline-separated-code
            (space-separated-code
              "always"
              (code "@"
                (code-in-round-brackets
                  (event->code #'event)))
              "begin")
            (code-indent
              (mutate->code $name #'body))
            "end")))))

  (define (mutate->code $name $syntax)
    (syntax-case $syntax (%if)
      ((%if body ...)
        (name-if->code $name $syntax))
      (other
        (name-set->code $name $syntax))))

  (define (name-if->code $name $if)
    (syntax-case $if (%if)
      ((%if cond expr)
        (newline-separated-code
          (space-separated-code
            "if"
            (code-in-round-brackets (expr->code #'cond))
            "begin")
          (code-indent
            (name-set->code $name #'expr))
          "end"))))

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
        ((from to)
          (code
            (index->code #'from)
            ":"
            (index->code #'to)))
        ((index)
          (index->code #'index)))))

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
