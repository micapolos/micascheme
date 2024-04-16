(import (scheme) (symbol) (check))

(check (equal? (symbol-append) (string->symbol "")))
(check (equal? (symbol-append 'a) 'a))
(check (equal? (symbol-append 'a 'b 'c) 'abc))
