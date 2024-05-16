(import (scheme) (check) (let))

(check (equal? (let-values/optimize (((a) 1) ((b) 2)) (+ a b)) 3))
(check (equal? (let-values/optimize (((a) 1) ((b c) (values 2 3))) (+ a b c)) 6))
