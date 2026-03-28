(import (micascheme) (leo quotes))

(check (equal? (char->quote? #\') 'quote))
(check (equal? (char->quote? #\`) 'quasiquote))
(check (equal? (char->quote? #\,) #f))

(check (equal? (char->unquote? #\`) 'unquote))
(check (equal? (char->unquote? #\,) #f))
