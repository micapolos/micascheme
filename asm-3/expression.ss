(library (asm-3 expression)
  (export
    expression? expression-deps expression-syntax
    pure-expression
    dep-expression
    combine-expressions
    check-expression
    expression->datum
    expression-with)
  (import (micascheme))

  (data (expression deps syntax))

  (define-rule-syntax (expression-with (dep ...) body)
    (expression (list #'dep ...) #'body))

  (define (pure-expression $syntax)
    (expression (list) $syntax))

  (define (dep-expression $dep)
    (expression (list $dep) $dep))

  (define (combine-expressions $proc $expressions)
    (expression
      (dedup free-identifier=? (apply append (map expression-deps $expressions)))
      ($proc (map expression-syntax $expressions))))

  (define (expression->datum $expression)
    `(expression
      (,@(map syntax->datum (expression-deps $expression)))
      ,(syntax->datum (expression-syntax $expression))))

  (define-rule-syntax (check-expression in out)
    (check (equal? (expression->datum in) 'out)))
)
