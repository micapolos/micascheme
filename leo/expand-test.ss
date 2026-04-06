(import (micascheme) (leo expand))

(check
  (syntax=?
    (leo-expand-once '(import (foo (bar goo))))
    '(import (foo bar goo))))

(check
  (syntax=?
    (leo-expand-once
      '(library
        (foo (bar goo))
        (export a b c)
        (import
          (zoo (zar zoo))
          (moo (mar moo)))
        a b c))
    '(library
      (foo bar goo)
      (export a b c)
      (import
        (zoo zar zoo)
        (moo mar moo))
      a b c)))
