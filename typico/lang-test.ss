(import (typico lang))

; === types
(check-equal? syntax syntax)
(check-equal? datum datum)
(check-equal? boolean boolean)
(check-equal? integer integer)
(check-equal? char char)
(check-equal? string string)
(check-equal? bytevector bytevector)
(check-equal? (list-of integer) (list-of integer))
(check-equal? (list-of (list-of integer)) (list-of (list-of integer)))

(check-equal? (function () string) (function () string))
(check-equal? (function (string integer) boolean) (function (string integer) boolean))
(check-equal? (function (integer ...) boolean) (function (integer ...) boolean))
(check-equal? (function (string integer ...) boolean) (function (string integer ...) boolean))

; === literals
(check-equal? #f #f)
(check-equal? #t #t)
(check-equal? 123 123)
(check-equal? #\a #\a)
(check-equal? "foo" "foo")

; === syntax/datum
(check-equal? (datum 123) (datum 123))
(check-works (syntax 123))
(check-equal? (datum (syntax 123)) 123)

; === list
(check-raises (empty-list-of 1))
(check-equal? (empty-list-of integer) (empty-list-of integer))

(check-raises (list))
(check-equal? (list 10) (list 10))
(check-equal? (list 1 2 3) (list 1 2 3))
(check-raises (list 1 #f))

; === bytevector
(check-equal? (bytevector) (bytevector))
(check-equal? (bytevector 1 2 3) (bytevector 1 2 3))
(check-raises (bytevector #x100))

; === typeof
(check-equal? (typeof #t) boolean)
(check-equal? (typeof 123) integer)
(check-equal? (typeof #\a) char)
(check-equal? (typeof "foo") string)
(check-equal? (typeof (syntax 123)) syntax)
(check-equal? (typeof (datum 123)) datum)

(check-primitive boolean=? boolean=?)
(check-primitive integer=? =)
(check-primitive char=? char=?)
(check-primitive string=? string=?)
(check-primitive bytevector=? bytevector=?)

(check-equal? (typeof boolean=?) (function (boolean boolean) boolean))
(check-equal? (typeof integer=?) (function (integer integer) boolean))
(check-equal? (typeof char=?) (function (char char) boolean))
(check-equal? (typeof string=?) (function (string string) boolean))
(check-equal? (typeof bytevector=?) (function (bytevector bytevector) boolean))

; === if

(check-equal? (if #t "true" "false") "true")
(check-equal? (if #f "true" "false") "false")
(check-raises (if 123 "true" "false"))
(check-raises (if #t "true" #f))

(check-raises =)
(check-raises (=))

(check-equal? (= #f #f) #t)
(check-equal? (= #f #t) #f)
(check-raises (= #f 1))

(check-equal? (= 1 1) #t)
(check-equal? (= 1 2) #f)
(check-raises (= 1 #f))

(check-equal? (= #\a #\a) #t)
(check-equal? (= #\a #\b) #f)
(check-raises (= #\a #f))

(check-equal? (= "a" "a") #t)
(check-equal? (= "a" "b") #f)
(check-raises (= "a" #f))

(check-equal? (+ 1) 1)
(check-equal? (+ 1 2 3) 6)

(check-equal? (+ "foo") "foo")
(check-equal? (+ "f" "o" "o") "foo")

(check-primitive integer+ +)
(check-equal? (typeof integer+) (function (integer ...) integer))

(check-equal? (integer+) 0)
(check-equal? (integer+ 1 2 3) 6)
(check-raises (integer+ #t))

(check-primitive string+ string-append)
(check-equal? (typeof string+) (function (string ...) string))

(check-equal? (string+) "")
(check-equal? (string+ "a" "b" "c") "abc")
(check-raises (string+ #t))

(check-raises +)
(check-raises (+))
(check-raises (+ #t))
(check-raises (+ #\a))
(check-raises (+ 1 #t))
(check-raises (+ "foo" #t))

(check-equal? (and #f) #f)
(check-equal? (and #t) #t)
(check-equal? (and #f #t #f) #f)
(check-equal? (and #t #t #t) #t)

(check-equal? (and #b000) #b000)
(check-equal? (and #b1110 #b1111 #b1100) #b1100)

(check-raises and)
(check-raises (and))
(check-raises (and #\a))
(check-raises (and "foo"))
(check-raises (and #f 1))
(check-raises (and 1 #f))

(check-equal? (let () 123) 123)
(check-equal? (let ((x 10)) x) 10)
(check-equal? (let ((x 10)) (+ x x)) 20)
(check-equal? (let ((x 10) (y 20)) (+ x y)) 30)

(check-equal? (typeof (lambda () 10)) (function () integer))
(check-equal? (typeof (lambda ((integer i) (string s)) i)) (function (integer string) integer))
(check-equal? (typeof (lambda ((integer i) (string s)) s)) (function (integer string) string))
;(check-equal? (typeof (lambda ((integer is) ...) is)) (function (integer ...) (list-of integer)))
(check-equal? (typeof (lambda ((string s) (integer is) ...) s)) (function (string integer ...) string))

(check-equal? ((lambda () 10)) 10)
(check-equal? ((lambda ((integer x)) x) 10) 10)
(check-equal? ((lambda ((integer x) (integer y)) (+ x y)) 10 20) 30)
(check-equal? ((lambda ((integer xs) ...) xs)) (empty-list-of integer))
;(check-equal? ((lambda ((integer xs) ...) xs) 1 2 3) (list 1 2 3))

(check-equal? (integer->string 123) "123")
(check-equal? (string-length "foo") 3)
