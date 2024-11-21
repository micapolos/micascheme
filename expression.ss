(library (expression)
  (export
    expression expression? expression-priority expression-left-to-right? expression-value
    unary-expression
    binary-expression)
  (import (micascheme))

  (data (expression priority left-to-right? value))

  (define (value-expression $value)
    (expression 0 #t $value))

  (define (expression-operand-value $promote $priority $right? $expression)
    (if
      (or
        (< (expression-priority $expression) $priority)
        (and
          (= (expression-priority $expression) $priority)
          (boolean=? (expression-left-to-right? $expression) (not $right?))))
      (expression-value $expression)
      ($promote (expression-value $expression))))

  (define (unary-expression $make $promote $priority $left-to-right? $rhs)
    (expression $priority $left-to-right?
      ($make
        (expression-operand-value $promote $priority #t $rhs))))

  (define (binary-expression $make $promote $priority $left-to-right? $lhs $rhs)
    (expression $priority $left-to-right?
      ($make
        (expression-operand-value $promote $priority #f $lhs)
        (expression-operand-value $promote $priority #t $rhs))))
)
