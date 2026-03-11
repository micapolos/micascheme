(import (micascheme) (leo expand))

(check
  (equal?
    (leo-expand-once '(define (x 10)))
    '(define x 10)))

(check
  (equal?
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
