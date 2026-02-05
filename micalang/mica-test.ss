(import (micalang base) (micalang mica))

(check-mica 1 1)
(check-mica (inc 1) 2)
(check-mica (+ 1 2) 3)

(check-mica (let (x 10) (y 20) (+ x y)) 30)

(check-mica ((lambda (x int) (inc x)) 2) 3)
(check-mica ((lambda (x int) (y int) (+ x y)) 2 3) 5)
