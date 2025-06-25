(import (micascheme) (typico type))

(check (type? (primitive-type (gensym) 'integer)))
(check (type? (function-type (list) (primitive-type (gensym) 'integer))))
(check (type? (expander-type (lambda ($lookup $syntax) (void)))))
(check (not (type? 123)))

(check
  (equal?
    (type->datum (primitive-type (gensym) 'foo))
    'foo))

(check
  (equal?
    (type->datum
      (function-type
        (list)
        (primitive-type (gensym) 'result)))
    '(function () result)))

(check
  (equal?
    (type->datum
      (function-type
        (list
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'result)))
    '(function (a b) result)))

(check
  (equal?
    (type->datum
      (function-type
        (list* (primitive-type (gensym) 'a))
        (primitive-type (gensym) 'result)))
    '(function (a ...) result)))

(check
  (equal?
    (type->datum
      (function-type
        (list*
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'result)))
    '(function (a b ...) result)))

(check
  (equal?
    (type->datum (expander-type identity))
    `(expander ,identity)))
