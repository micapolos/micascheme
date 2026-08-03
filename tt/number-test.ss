(import (tt lang) (tt number) (tt boolean))

(check (number=? 1 1))
(check (number=? (+ 1 2) 3))
(check (number=? (- 3 2) 1))
(check (zero? 0))
(check (not (zero? 1)))

(check (boolean=? (= 1 1) #t))
(check (boolean=? (= 1 2) #f))
