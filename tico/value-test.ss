(import (micascheme) (tico value))

;(check (equal? (tuple-value (list)) #f))
(check (equal? (tuple-value (list "foo")) "foo"))
(check (equal? (tuple-value (list "foo" "bar")) (cons "foo" "bar")))
(check (equal? (tuple-value (list "foo" "bar" "zoo")) (vector "foo" "bar" "zoo")))

(check (equal? (tuple-ref-value 1 128 0) 128))
(check (equal? (tuple-ref-value 2 (cons "foo" "bar") 0) "foo"))
(check (equal? (tuple-ref-value 2 (cons "foo" "bar") 0) "foo"))
(check (equal? (tuple-ref-value 3 (vector "foo" "bar" "zoo") 0) "foo"))
(check (equal? (tuple-ref-value 3 (vector "foo" "bar" "zoo") 1) "bar"))
(check (equal? (tuple-ref-value 3 (vector "foo" "bar" "zoo") 2) "zoo"))

(check (equal? (app (value-abstraction 2 "foo") 1 2) "foo"))
