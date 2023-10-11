(import (micascheme) (mico-c))

(check
  (equal?
    (syntax->type #`int)
    `int))

(check
  (equal?
    (syntax->type #`(function (int string) int))
    (arrow (list `int `string) `int)))

(check
  (equal?
    (syntax->typed-c #`(c "foo" int))
    (typed "foo" `int)))

(check
  (equal?
    (syntax->typed-c
      #`((c "add" (function (int int) int)) 1 2))
    (typed "add(1, 2)" `int)))
