(library (asm-3 expression)
  (export
    pure-expression
    dep-expression
    combine-expressions
    check-expression
    expression-with)
  (import (micascheme))

  (data (expression deps syntax))

  (define-rule-syntax (expression-with (dep ...) body)
    (expression (stack #'dep ...) #'body))

  (define (pure-expression $syntax)
    (expression (stack) $syntax))

  (define (dep-expression $dep)
    (expression (stack $dep) $dep))

  (define (combine-expressions $proc $expressions)
    (expression
      (reverse (dedup free-identifier=? (apply append (map (dot reverse expression-deps) $expressions))))
      ($proc (map expression-syntax $expressions))))

  (define (expression->datum $expression)
    `(expression-with
      (,@(reverse (map syntax->datum (expression-deps $expression))))
      ,(syntax->datum (expression-syntax $expression))))

  (define-rule-syntax (check-expression in out)
    (check (equal? (expression->datum in) 'out)))
)
