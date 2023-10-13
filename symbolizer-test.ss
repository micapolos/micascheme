(import (micascheme) (symbolizer))

(define foo-symbol (symbolize "foo"))

(check (equal? (symbolized-bound? `foo) #f))
(check (equal? (symbolized-bound? `string-append) #t))
(check (equal? (symbolized-bound? foo-symbol) #t))

(check (equal? (symbolized-value foo-symbol) "foo"))
(check (equal? (symbolized-value `string-append) string-append))

(check (equal? (symbolized-eval `(string-append ,foo-symbol "!")) "foo!"))
