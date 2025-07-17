(import (micascheme) (asm-3 expression))

(check-expression
  (pure-expression #'string-append)
  (expression () string-append))

(check-expression
  (dep-expression #'foo)
  (expression (foo) foo))

(check-expression
  (combine-expressions
    (lambda ($syntaxes)
      #`(combined #,@$syntaxes))
    (list
      (expression-with (a b) (+ a b))
      (expression-with (b c) (+ b c))
      (expression-with (a d) (+ a d))))
  (expression
    (a b c d)
    (combined (+ a b) (+ b c) (+ a d))))
