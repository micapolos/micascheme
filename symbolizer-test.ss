(import (micascheme) (symbolizer))

(define foo-symbol (symbolize "foo"))

(check (equal? (symbolized-value foo-symbol) "foo"))
(check (equal? (symbolized-eval `(string-append ,foo-symbol "!")) "foo!"))
