(import (micascheme) (typed lang) (typed type))

(check (equal? (tt any-boolean) any-boolean))
(check (equal? (tt any-number) any-number))
(check (equal? (tt any-string) any-string))
(check (equal? (tt any-syntax) any-syntax))

(check (equal? (tt #f) #f))
(check (equal? (tt 123) 123))
(check (equal? (tt "foo") "foo"))
(check (syntax=? (tt #'(foo bar)) #'(foo bar)))

