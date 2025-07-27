(import (micascheme) (monoid))

(check (equal? (monoid-empty string) ""))
(check (equal? (monoid-append string ) ""))
(check (equal? (monoid-append string "a" "b" "c") "abc"))

(check (equal? (monoid-empty list) '()))
(check (equal? (monoid-append list) '()))
(check (equal? (monoid-append list '(1) '(2 3) '(4 5 6)) '(1 2 3 4 5 6)))
