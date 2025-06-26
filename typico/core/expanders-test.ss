(import (typico base) (typico expander) (typico core expanders))

(check-expand-core #f (typed boolean #f))
(check-expand-core 10 (typed integer 10))
(check-expand-core #\a (typed char #\a))
(check-expand-core "foo" (typed string "foo"))

(check-expand-core
  (if #t 10 20)
  (typed integer (if #t 10 20)))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

(check-expand-core (+ 1 2) (typed integer 3))
(check-expand-core (+ integer-0 integer-1) (typed integer (($primitive 3 +) 0 1)))
