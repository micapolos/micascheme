(import (micascheme) (tico datum) (tico type))

(check (equal? (value->datum #f) #f))
(check (equal? (value->datum 128) 128))
(check (equal? (value->datum "foo") "foo"))
(check (equal? (value->datum #\a) #\a))
(check (equal? (value->datum `()) `()))
(check (equal? (value->datum (cons 1 2)) `(cons 1 2)))
(check (equal? (value->datum (vector 1 2 3)) `(vector 1 2 3)))

(check (equal? (value->datum (any-type)) `(any-type)))
(check (equal? (value->datum (native-type)) `(native-type)))
(check (equal? (value->datum (type-type)) `(type-type)))
(check (equal? (value->datum (value-type "foo")) `(value-type "foo")))

(check 
	(equal? 
		(value->datum (struct 'foo (list "foo" "bar")))
		`(struct 'foo (list "foo" "bar"))))

(check 
	(equal? 
		(value->datum (lambda-type (list "foo" "bar") "zoo"))
		`(lambda-type (list "foo" "bar") "zoo")))
