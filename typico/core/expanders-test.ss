(import (typico base) (typico expander) (typico core expanders))

(check-expand-core #f (typed boolean #f))
(check-expand-core 10 (typed integer 10))
(check-expand-core #\a (typed char #\a))
(check-expand-core "foo" (typed string "foo"))

(check-expand-core (if #t 10 20) (typed integer 10))
(check-expand-core (if #f 10 20) (typed integer 20))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

(check-expand-core-raises (+))

(check-expand-core (+ 1) (typed integer 1))
(check-expand-core (+ 1 2) (typed integer 3))
(check-expand-core (+ 1 2 3) (typed integer 6))
(check-expand-core (+ integer-zero integer-one) (typed integer 1))

(check-expand-core (+ "a") (typed string "a"))
(check-expand-core (+ "a" "b") (typed string "ab"))
(check-expand-core (+ "a" "b" "c") (typed string "abc"))

(check-expand-core (u8 0) (typed u8 0))
(check-expand-core (u8 255) (typed u8 255))
(check-expand-core-raises (u8 -1))
(check-expand-core-raises (u8 256))
(check-expand-core (u8 (+ integer-one 2)) (typed u8 3))
(check-expand-core (u8 (+ 1 (+ 2 3))) (typed u8 6))

(check-expand-core (= #f #f) (typed boolean #t))
(check-expand-core (= #f #t) (typed boolean #f))

(check-expand-core (= 1 1) (typed boolean #t))
(check-expand-core (= 1 2) (typed boolean #f))

(check-expand-core (= #\a #\a) (typed boolean #t))
(check-expand-core (= #\a #\b) (typed boolean #f))

(check-expand-core (= "a" "a") (typed boolean #t))
(check-expand-core (= "a" "b") (typed boolean #f))
