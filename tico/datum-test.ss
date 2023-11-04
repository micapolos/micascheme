(import (micascheme) (tico datum) (tico type))

(check
	(equal?
		(datum-application 'fn (list 'v1 'v2))
		'(fn v1 v2)))

(check
	(equal?
		(with-generate-temporary-seed $tmp
			(datum-params 2))
		'($tmp-0 $tmp-1)))

(check
	(equal?
		(datum-abstraction (list 'v1 'v2) '(+ v1 v2))
		'(lambda (v1 v2) (+ v1 v2))))

(check
	(equal?
		(with-generate-temporary-seed $tmp
			(generate-datum-abstraction 2
				(lambda ($params)
					`(done ,@$params))))
		(datum-abstraction
			(list '$tmp-0 '$tmp-1)
			'(done $tmp-0 $tmp-1))))

(check (equal? (datum->value '(string-append "foo" "bar")) "foobar"))

(check	(equal? (datum-tuple (list)) #f))
(check	(equal? (datum-tuple (list 'v1)) 'v1))
(check	(equal? (datum-tuple (list 'v1 'v2)) '(cons v1 v2)))
(check	(equal? (datum-tuple (list 'v1 'v2 'v3)) '(vector v1 v2 v3)))

(check	(equal? (arity-datum-ref 1 'v 0) 'v))
(check	(equal? (arity-datum-ref 2 'v 0) '(car v)))
(check	(equal? (arity-datum-ref 2 'v 1) '(cdr v)))
(check	(equal? (arity-datum-ref 3 'v 0) '(vector-ref v 0)))
(check	(equal? (arity-datum-ref 3 'v 1) '(vector-ref v 1)))
(check	(equal? (arity-datum-ref 3 'v 2) '(vector-ref v 2)))

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
		(value->datum (arrow (list "foo" "bar") "zoo"))
		`(arrow (list "foo" "bar") "zoo")))

(check
	(equal?
		(lets-datum (list) 'c)
		'c))

(check
	(equal?
		(lets-datum (list '(a 1) '(b 2)) 'c)
		'(lets (a 1) (b 2) c)))
