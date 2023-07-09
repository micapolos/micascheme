(import (base) (base-syntax))

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))

; === define-data-constructor ===

(define-data-constructor (tuple0))
(define-data-constructor (tuple1 field1))
(define-data-constructor (tuple2 field1 field2))
(define-data-constructor (tuple3 field1 field2 field3))

(check (equal? (tuple0) #f))
(check (equal? (tuple1 "foo") "foo"))
(check (equal? (tuple2 "foo" 128) (cons "foo" 128)))
(check (equal? (tuple3 "foo" 128 #t) (vector "foo" 128 #t)))

; === define-data-accessors ===

(define-data-accessors (tuple0))
(define-data-accessors (tuple1 field1))
(define-data-accessors (tuple2 field1 field2))
(define-data-accessors (tuple3 field1 field2 field3))

(check (equal? (tuple1-field1 (tuple1 "foo")) "foo"))
(check (equal? (tuple2-field1 (tuple2 "foo" 128)) "foo"))
(check (equal? (tuple2-field2 (tuple2 "foo" 128)) 128))
(check (equal? (tuple3-field1 (tuple3 "foo" 128 #t)) "foo"))
(check (equal? (tuple3-field2 (tuple3 "foo" 128 #t)) 128))
(check (equal? (tuple3-field3 (tuple3 "foo" 128 #t)) #t))
