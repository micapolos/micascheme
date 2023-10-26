(import (micascheme) (tico type))

(check (equal? (type-dynamic? (value-type "foo")) #f))
(check (equal? (type-dynamic? (boolean-type)) #t))
(check (equal? (type-dynamic? (number-type)) #t))
(check (equal? (type-dynamic? (string-type)) #t))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (value-type "bar")))) #f))
(check (equal? (type-dynamic? (struct-type `foo (list (value-type "foo") (number-type)))) #t))
(check (equal? (type-dynamic? (lambda-type (list (number-type)) (number-type))) #t))
(check (equal? (type-dynamic? (lambda-type (list (number-type)) (value-type "foo"))) #f))
(check (raises? (lambda () (type-dynamic? `not-type))))