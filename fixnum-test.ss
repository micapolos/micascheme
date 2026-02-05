(import (scheme) (check) (fixnum))

(check (equal? (fx+1/wraparound (most-positive-fixnum)) (most-negative-fixnum)))
(check (equal? (fx-1/wraparound (most-negative-fixnum)) (most-positive-fixnum)))
