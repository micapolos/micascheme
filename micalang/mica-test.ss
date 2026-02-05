(import (micalang base) (micalang mica))

(check-mica 1 1)
(check-mica (zero? 0) #t)
(check-mica (zero? 1) #f)
(check-mica (inc 1) 2)
(check-mica (dec 2) 1)
(check-mica (+ 1 2) 3)
(check-mica (- 3 2) 1)
(check-mica (< 1 2) #t)
(check-mica (< 2 1) #f)

(check-mica (let (x 10) (y 20) (+ x y)) 30)

(check-mica ((lambda (x int) (inc x)) 2) 3)
(check-mica ((lambda (x int) (y int) (+ x y)) 2 3) 5)
