(import (base) (base-syntax))

(check (equal? (index-switch 0 "zero" "one" "two") "zero"))
(check (equal? (index-switch 1 "zero" "one" "two") "one"))
(check (equal? (index-switch 2 "zero" "one" "two") "two"))
(check (equal? (index-switch 3 "zero" "one" "two") "two"))

; === define-struct-constructor ===

(define-struct-constructor (tuple0))
(define-struct-constructor (tuple1 string))
(define-struct-constructor (tuple2 string number))
(define-struct-constructor (tuple3 string number tuple0))

(check (equal? (tuple0) #f))
(check (equal? (tuple1 "foo") "foo"))
(check (equal? (tuple2 "foo" 128) (cons "foo" 128)))
(check (equal? (tuple3 "foo" 128 (tuple0)) (vector "foo" 128 (tuple0))))

; === define-struct-accessors ===

(define-struct-accessors (tuple0))
(define-struct-accessors (tuple1 string))
(define-struct-accessors (tuple2 string number))
(define-struct-accessors (tuple3 string number tuple0))

(check (equal? (tuple1-string (tuple1 "foo")) "foo"))
(check (equal? (tuple2-string (tuple2 "foo" 128)) "foo"))
(check (equal? (tuple2-number (tuple2 "foo" 128)) 128))
(check (equal? (tuple3-string (tuple3 "foo" 128 (tuple0))) "foo"))
(check (equal? (tuple3-number (tuple3 "foo" 128 (tuple0))) 128))
(check (equal? (tuple3-tuple0 (tuple3 "foo" 128 (tuple0))) (tuple0)))

; === define-struct->datum ===

(check (equal? (boolean->datum #t) #t))
(check (equal? (number->datum 128) 128))
(check (equal? (string->datum "foo") "foo"))

(define-struct->datum (tuple0))
(define-struct->datum (tuple1 string))
(define-struct->datum (tuple2 string number))
(define-struct->datum (tuple3 string number tuple0))

(check (equal? (tuple0->datum (tuple0)) `(tuple0)))
(check (equal? (tuple1->datum (tuple1 "foo")) `(tuple1 "foo")))
(check (equal? (tuple2->datum (tuple2 "foo" 128)) `(tuple2 "foo" 128)))
(check (equal? (tuple3->datum (tuple3 "foo" 128 (tuple0))) `(tuple3 "foo" 128 (tuple0))))

; === define-struct ===

(define-struct (struct0))
(define-struct (struct1 string))
(define-struct (struct2 string number))
(define-struct (struct3 string number struct0))

(check (equal? (struct0) #f))
(check (equal? (struct1 "foo") "foo"))
(check (equal? (struct2 "foo" 128) (cons "foo" 128)))
(check (equal? (struct3 "foo" 128 (struct0)) (vector "foo" 128 (struct0))))

(check (equal? (struct1-string (struct1 "foo")) "foo"))
(check (equal? (struct2-string (struct2 "foo" 128)) "foo"))
(check (equal? (struct2-number (struct2 "foo" 128)) 128))
(check (equal? (struct3-string (struct3 "foo" 128 (struct0))) "foo"))
(check (equal? (struct3-number (struct3 "foo" 128 (struct0))) 128))
(check (equal? (struct3-struct0 (struct3 "foo" 128 (struct0))) (struct0)))

(check (equal? (struct0->datum (struct0)) `(struct0)))
(check (equal? (struct1->datum (struct1 "foo")) `(struct1 "foo")))
(check (equal? (struct2->datum (struct2 "foo" 128)) `(struct2 "foo" 128)))
(check (equal? (struct3->datum (struct3 "foo" 128 (struct0))) `(struct3 "foo" 128 (struct0))))

; === define-one-of-constructor ===

(define-one-of-constructor (one-of-3 string number struct0))

(check (equal? (one-of-3 "foo" (not number) (not struct0)) (cons 0 "foo")))
(check (equal? (one-of-3 (not string) 128 (not struct0)) (cons 1 128)))
(check (equal? (one-of-3 (not string) (not number) (struct0)) (cons 2 (struct0))))
