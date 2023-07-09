(import (base) (base-syntax))

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))

; === define-data-constructor ===

(define-data-constructor (tuple0))
(define-data-constructor (tuple1 string))
(define-data-constructor (tuple2 string number))
(define-data-constructor (tuple3 string number tuple0))

(check (equal? (tuple0) #f))
(check (equal? (tuple1 "foo") "foo"))
(check (equal? (tuple2 "foo" 128) (cons "foo" 128)))
(check (equal? (tuple3 "foo" 128 (tuple0)) (vector "foo" 128 (tuple0))))

; === define-data-accessors ===

(define-data-accessors (tuple0))
(define-data-accessors (tuple1 string))
(define-data-accessors (tuple2 string number))
(define-data-accessors (tuple3 string number tuple0))

(check (equal? (tuple1-string (tuple1 "foo")) "foo"))
(check (equal? (tuple2-string (tuple2 "foo" 128)) "foo"))
(check (equal? (tuple2-number (tuple2 "foo" 128)) 128))
(check (equal? (tuple3-string (tuple3 "foo" 128 (tuple0))) "foo"))
(check (equal? (tuple3-number (tuple3 "foo" 128 (tuple0))) 128))
(check (equal? (tuple3-tuple0 (tuple3 "foo" 128 (tuple0))) (tuple0)))

; === define-data->datum ===

(check (equal? (boolean->datum #t) #t))
(check (equal? (number->datum 128) 128))
(check (equal? (string->datum "foo") "foo"))

(define-data->datum (tuple0))
(define-data->datum (tuple1 string))
(define-data->datum (tuple2 string number))
(define-data->datum (tuple3 string number tuple0))

(check (equal? (tuple0->datum (tuple0)) `(tuple0)))
(check (equal? (tuple1->datum (tuple1 "foo")) `(tuple1 "foo")))
(check (equal? (tuple2->datum (tuple2 "foo" 128)) `(tuple2 "foo" 128)))
(check (equal? (tuple3->datum (tuple3 "foo" 128 (tuple0))) `(tuple3 "foo" 128 (tuple0))))

; === define-data ===

(define-data (data0))
(define-data (data1 string))
(define-data (data2 string number))
(define-data (data3 string number data0))

(check (equal? (data0) #f))
(check (equal? (data1 "foo") "foo"))
(check (equal? (data2 "foo" 128) (cons "foo" 128)))
(check (equal? (data3 "foo" 128 (data0)) (vector "foo" 128 (data0))))

(check (equal? (data1-string (data1 "foo")) "foo"))
(check (equal? (data2-string (data2 "foo" 128)) "foo"))
(check (equal? (data2-number (data2 "foo" 128)) 128))
(check (equal? (data3-string (data3 "foo" 128 (data0))) "foo"))
(check (equal? (data3-number (data3 "foo" 128 (data0))) 128))
(check (equal? (data3-data0 (data3 "foo" 128 (data0))) (data0)))

(check (equal? (data0->datum (data0)) `(data0)))
(check (equal? (data1->datum (data1 "foo")) `(data1 "foo")))
(check (equal? (data2->datum (data2 "foo" 128)) `(data2 "foo" 128)))
(check (equal? (data3->datum (data3 "foo" 128 (data0))) `(data3 "foo" 128 (data0))))

; === define-one-of-constructor ===

(define-one-of-constructor (one-of-3 string number data0))

(check (equal? (one-of-3 "foo" (not number) (not data0)) (cons 0 "foo")))
(check (equal? (one-of-3 (not string) 128 (not data0)) (cons 1 128)))
(check (equal? (one-of-3 (not string) (not number) (data0)) (cons 2 (data0))))
