(library (micac expr)
  (export
    expr expr? expr-priority expr-left-to-right? expr-code
    expr-operand-code
    binary-expr
    prefix-expr
    parenthesized-expr
    identifier->expr)
  (import
    (micascheme)
    (code)
    (micac code))

  (data (expr priority left-to-right? code))

  (define (identifier->expr $variable)
    (expr 0 #t (identifier->code $variable)))

  (define (expr-operand-code $expr $priority $right?)
    (if
      (or
        (< (expr-priority $expr) $priority)
        (and
          (= (expr-priority $expr) $priority)
          (boolean=? (expr-left-to-right? $expr) (not $right?))))
      (expr-code $expr)
      (code-in-round-brackets (expr-code $expr))))

  (define (binary-expr $priority $left-to-right? $lhs $op $rhs)
    (expr $priority $left-to-right?
      (code
        (expr-operand-code $lhs $priority #f)
        (string-code $op)
        (expr-operand-code $rhs $priority #t))))

  (define (prefix-expr $priority $left-to-right? $op $expr)
    (expr $priority $left-to-right?
      (code
        (string-code $op)
        (expr-operand-code $expr $priority #t))))

  (define (parenthesized-expr $priority $left-to-right? $lhs $lparen $inner $rparen)
    (expr $priority $left-to-right?
      (code
        (expr-operand-code $lhs $priority #f)
        (string-code $lparen)
        (expr-code $inner)
        (string-code $rparen))))
)
