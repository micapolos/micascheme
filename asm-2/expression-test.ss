(import (micascheme) (asm-2 expression))

(check
  (equal?
    (expression->value
      (expression-with 10)
      100)
    10))

(check
  (equal?
    (expression->value
      (expression-with ($org) (+ $org 10))
      100)
    110))

(check
  (equal?
    (expression->value
      (expression-map
        (lambda ($value) (+ $value 1))
        (expression-with ($org) (+ $org 10)))
      100)
    111))
