(import (micascheme) (tico type))

(check (equal? (type-dynamic? (native-type)) #t))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (value-type "bar")))) #f))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (number-type)))) #t))
(check (equal? (type-dynamic? (lambda-type (list (value-type "foo")) (value-type "bar"))) #t))
(check (raises? (lambda () (type-dynamic? `not-type))))
