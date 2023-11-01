(import (micascheme) (tico datum))

(check (equal? (value->datum #f) #f))
(check (equal? (value->datum 128) 128))
(check (equal? (value->datum "foo") "foo"))
(check (equal? (value->datum #\a) #\a))
(check (equal? (value->datum `()) `()))
(check (equal? (value->datum (cons 1 2)) `(cons 1 2)))
(check (equal? (value->datum (vector 1 2 3)) `(vector 1 2 3)))
