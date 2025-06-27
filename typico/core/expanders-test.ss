(import (typico base) (typico expander) (typico core expanders))

; literals

(check-expand-core #f (boolean #f))
(check-expand-core 10 (integer 10))
(check-expand-core #\a (char #\a))
(check-expand-core "foo" (string "foo"))

; dynamic (non compile-time constant)

(check-expand-core
  (dynamic "foo")
  (string (dynamic "foo")))

; if

(check-expand-core
  (if #t 10 20)
  (integer 10))

(check-expand-core
  (if #f 10 20)
  (integer 20))

(check-expand-core
  (if (dynamic #t) 10 20)
  (integer (if (dynamic #t) 10 20)))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

; integer +
(check-expand-core-raises (+))
(check-expand-core (+ 1) (integer 1))
(check-expand-core (+ 1 2) (integer 3))
(check-expand-core (+ 1 2 3) (integer 6))
(check-expand-core (+ (dynamic 1) 2 3) (integer (($primitive 3 +) (dynamic 1) 2 3)))
(check-expand-core (+ integer-zero integer-one) (integer 1))

; string append
(check-expand-core-raises (append))
(check-expand-core (append "a") (string "a"))
(check-expand-core (append "a" "b") (string "ab"))
(check-expand-core (append "a" "b" "c") (string "abc"))
(check-expand-core
  (append (dynamic "a") "b" "c")
  (string (($primitive 3 string-append) (dynamic "a") "b" "c")))

; u8

(check-expand-core (u8 0) (u8 0))
(check-expand-core (u8 255) (u8 255))
(check-expand-core-raises (u8 -1))
(check-expand-core-raises (u8 256))
(check-expand-core (u8 (+ integer-one 2)) (u8 3))
(check-expand-core (u8 (+ 1 (+ 2 3))) (u8 6))

; boolean =

(check-expand-core (= #f #f) (boolean #t))
(check-expand-core (= #f #t) (boolean #f))
(check-expand-core
  (= (dynamic #f) #t)
  (boolean (($primitive 3 boolean=?) (dynamic #f) #t)))

; integer =

(check-expand-core (= 1 1) (boolean #t))
(check-expand-core (= 1 2) (boolean #f))
(check-expand-core
  (= (dynamic 1) 2)
  (boolean (($primitive 3 =) (dynamic 1) 2)))

; char =

(check-expand-core (= #\a #\a) (boolean #t))
(check-expand-core (= #\a #\b) (boolean #f))
(check-expand-core
  (= (dynamic #\a) #\b)
  (boolean (($primitive 3 char=?) (dynamic #\a) #\b)))

; string =

(check-expand-core (= "a" "a") (boolean #t))
(check-expand-core (= "a" "b") (boolean #f))
(check-expand-core
  (= (dynamic "a") "b")
  (boolean (($primitive 3 string=?) (dynamic "a") "b")))

; string-length

(check-expand-core (length "foo") (integer 3))
(check-expand-core
  (length (dynamic "foo"))
  (integer (($primitive 3 string-length) (dynamic "foo"))))

; string

(check-expand-core (string) (string ""))
(check-expand-core (string #\a #\b #\c) (string "abc"))
(check-expand-core
  (string (dynamic #\a) #\b #\c)
  (string (($primitive 3 string) (dynamic #\a) #\b #\c)))
