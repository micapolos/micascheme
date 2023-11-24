(import (micascheme) (tico datum) (tico type))

(check (equal? (test-datum foo) ''foo))

(check (equal? (literal->datum #f) #f))
(check (equal? (literal->datum 128) 128))
(check (equal? (literal->datum "foo") "foo"))
(check (equal? (literal->datum #\space) #\space))
(check (equal? (literal->datum 'foo) ''foo))

(check
  (equal?
  	(datum-args (list 'v1 'v2))
  	'(v1 v2)))

(check
  (equal?
  	(datum-args-application
  		'list
  		(datum-args (list 'v1 'v2)))
  	'(list v1 v2)))

(check
  (equal?
  	(datum-application 'fn (list 'v1 'v2))
  	'(fn v1 v2)))

(check
 	(equal?
	  (with-generate-temporary-seed $tmp
   		(generate-datum-params 2))
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

(check
	(equal?
		(bindings-datum->value
			(stack
				(cons 'foo "xxx")
				(cons 'bar "bar")
				(cons 'foo "foo"))
			'(string-append foo bar))
		"foobar"))

(check (raises? (lambda () (datum-tuple (list)))))
(check (equal? (datum-tuple (list 'v1)) 'v1))
(check (equal? (datum-tuple (list 'v1 'v2)) '(cons v1 v2)))
(check (equal? (datum-tuple (list 'v1 'v2 'v3)) '(vector v1 v2 v3)))

(check (equal? (tuple-ref-datum 1 'v 0) 'v))
(check (equal? (tuple-ref-datum 2 'v 0) '(car v)))
(check (equal? (tuple-ref-datum 2 'v 1) '(cdr v)))
(check (equal? (tuple-ref-datum 3 'v 0) '(vector-ref v 0)))
(check (equal? (tuple-ref-datum 3 'v 1) '(vector-ref v 1)))
(check (equal? (tuple-ref-datum 3 'v 2) '(vector-ref v 2)))

(check (equal? (value->datum #f) #f))
(check (equal? (value->datum 128) 128))
(check (equal? (value->datum "foo") "foo"))
(check (equal? (value->datum #\a) #\a))
(check (equal? (value->datum `()) `()))
(check (equal? (value->datum (cons 1 2)) `(cons 1 2)))
(check (equal? (value->datum (vector 1 2 3)) `(vector 1 2 3)))

(check (equal? (value->datum (any-type)) `(any-type)))
(check (equal? (value->datum (unchecked-type)) `(unchecked-type)))
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
	  (value->datum (property "foo" "zoo"))
  	`(property "foo" "zoo")))

(check
 	(equal?
	  (lets-datum (list) 'c)
  		'c))

(check
 	(equal?
	  (lets-datum (list '(a 1) '(b 2)) 'c)
		  '(lets (a 1) (b 2) c)))

(check
	(equal?
		(let-values-entry-datum
			(list 'p1 'p2 'p3)
			(list 'v1 'v2 'v3))
		'((p1 p2 p3) (values v1 v2 v3))))

(check
	(equal?
		(let-values-datum
			(list '((p1 p2) f1) '((p3 p4) f2))
			'foo)
		'(let-values
			(((p1 p2) f1) ((p3 p4) f2))
				foo)))
