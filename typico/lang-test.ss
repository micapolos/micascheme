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

(check-equal? (and #f) #f)
(check-equal? (and #t) #t)
(check-equal? (and #f #t #f) #f)
(check-equal? (and #t #t #t) #t)

(check-equal? (and #b000) #b000)
(check-equal? (and #b1110 #b1111 #b1100) #b1100)

(check-equal? (let () 123) 123)
(check-equal? (let ((x 10)) x) 10)
(check-equal? (let ((x 10)) (+ x x)) 20)
(check-equal? (let ((x 10) (y 20)) (+ x y)) 30)
