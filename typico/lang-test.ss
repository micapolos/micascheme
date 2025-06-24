(import (typico lang))

(check-equal? #f #f)
(check-equal? #t #t)
(check-equal? 123 123)
(check-equal? #\a #\a)
(check-equal? "foo" "foo")

(check-equal? (+ 1) 1)
(check-equal? (+ 1 2 3) 6)

(check-equal? (+ "foo") "foo")
(check-equal? (+ "f" "o" "o") "foo")

(check-primitive integer+ +)
(check-equal? (integer+) 0)
(check-equal? (integer+ 1 2 3) 6)

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
