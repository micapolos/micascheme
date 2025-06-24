(import (micascheme) (typico type) (typico typed))

(check
  (equal?
    (typed->datum
      (typed
        (primitive-type (gensym) 'integer)
        '123))
    '(typed integer 123)))
