(import (micascheme) (tico value))

(check (equal? (tuple-value (list)) #f))
(check (equal? (tuple-value (list "foo")) "foo"))
(check (equal? (tuple-value (list "foo" "bar")) (cons "foo" "bar")))
(check (equal? (tuple-value (list "foo" "bar" "zoo")) (vector "foo" "bar" "zoo")))
