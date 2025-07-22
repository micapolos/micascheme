(import (asm-3 base) (asm-3 expression) (asm-3 expression-syntax))

(define-expr add +)

(check-expression #xc000
  (empty-lookup)
  (expr 123)
  (expression (dependent 123)))

(check-expression #xc000
  (lookup-with (foo 10))
  (expr foo)
  (expression (dependent (foo) 10)))

(check-expression #xc000
  (empty-lookup)
  (expr (add 10 20))
  (expression (dependent 30)))

(check-expression #xc000
  (lookup-with (foo 10) (bar 20))
  (expr (add foo bar))
  (expression (dependent (foo bar) 30)))

(check-expression #xc000
  (empty-lookup)
  (expr org)
  (expression (dependent #xc000)))

(check-expression #xc000
  (empty-lookup)
  (expr (add org #x10))
  (expression (dependent #xc010)))

