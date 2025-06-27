(import (typico base) (typico expander) (typico core expanders))

(check-expand-core #f (typed boolean #f))
(check-expand-core 10 (typed integer 10))
(check-expand-core #\a (typed char #\a))
(check-expand-core "foo" (typed string "foo"))

(check-expand-core (dynamic "foo") (typed string (dynamic "foo")))

(check-expand-core (if #t 10 20) (typed integer 10))
(check-expand-core (if #f 10 20) (typed integer 20))

(check-expand-core
  (if (dynamic #t) 10 20)
  (typed integer (if (dynamic #t) 10 20)))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

(check-expand-core-raises (+))

(check-expand-core (+ 1) (typed integer 1))
(check-expand-core (+ 1 2) (typed integer 3))
(check-expand-core (+ 1 2 3) (typed integer 6))
(check-expand-core (+ (dynamic 1) 2 3) (typed integer (($primitive 3 +) (dynamic 1) 2 3)))
(check-expand-core (+ integer-zero integer-one) (typed integer 1))

(check-expand-core (+ "a") (typed string "a"))
(check-expand-core (+ "a" "b") (typed string "ab"))
(check-expand-core (+ "a" "b" "c") (typed string "abc"))
(check-expand-core
  (+ (dynamic "a") "b" "c")
  (typed string (($primitive 3 string-append) (dynamic "a") "b" "c")))

(check-expand-core (u8 0) (typed u8 0))
(check-expand-core (u8 255) (typed u8 255))
(check-expand-core-raises (u8 -1))
(check-expand-core-raises (u8 256))
(check-expand-core (u8 (+ integer-one 2)) (typed u8 3))
(check-expand-core (u8 (+ 1 (+ 2 3))) (typed u8 6))

(check-expand-core (= #f #f) (typed boolean #t))
(check-expand-core (= #f #t) (typed boolean #f))
(check-expand-core
  (= (dynamic #f) #t)
  (typed boolean (($primitive 3 boolean=?) (dynamic #f) #t)))

(check-expand-core (= 1 1) (typed boolean #t))
(check-expand-core (= 1 2) (typed boolean #f))
(check-expand-core
  (= (dynamic 1) 2)
  (typed boolean (($primitive 3 =) (dynamic 1) 2)))

(check-expand-core (= #\a #\a) (typed boolean #t))
(check-expand-core (= #\a #\b) (typed boolean #f))
(check-expand-core
  (= (dynamic #\a) #\b)
  (typed boolean (($primitive 3 char=?) (dynamic #\a) #\b)))

(check-expand-core (= "a" "a") (typed boolean #t))
(check-expand-core (= "a" "b") (typed boolean #f))
(check-expand-core
  (= (dynamic "a") "b")
  (typed boolean (($primitive 3 string=?) (dynamic "a") "b")))
