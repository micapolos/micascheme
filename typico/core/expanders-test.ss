(import (typico base) (typico expander) (typico core expanders) (typico type) (typico core types) (typico typed))

; primitive types

(check-expand-core-type boolean boolean-type)
(check-expand-core-type integer integer-type)
(check-expand-core-type char    char-type)
(check-expand-core-type string  string-type)
(check-expand-core-type datum   datum-type)

; function type

(check-expand-core-type
  (function (string char) integer)
  (function-type (list string-type char-type) integer-type))

(check-expand-core-type
  (function (string char ...) integer)
  (function-type (list* string-type char-type) integer-type))

(check-expand-core-raises (function (string char)))
(check-expand-core-raises (function (string char) foo))
(check-expand-core-raises (function (string char) integer integer))
(check-expand-core-raises (function (string foo) integer))

; function

(check-expand-core
  (lambda ((integer i) (string s)) i)
  (
    (function (integer string) integer)
    (lambda (i s) i)))

(check-expand-core
  (lambda ((integer i) (string s)) s)
  (
    (function (integer string) string)
    (lambda (i s) s)))

(check-expand-core
  (lambda ((integer i) (string s) ...) i)
  (
    (function (integer string ...) integer)
    (lambda (i . s) i)))

; TODO: list-of type
; (check-expand-core
;   (lambda ((integer i) (string s) ...) s)
;   (
;     (function (integer string ...) (list-of string))
;     (lambda (i . s) s)))

; literals

(check-expand-core #f (boolean #f))
(check-expand-core 10 (integer 10))
(check-expand-core #\a (char #\a))
(check-expand-core "foo" (string "foo"))

(check-expand-core-raises 10.0)  ; integers must be exact

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

; let

(check-expand-core
  (let 10)
  (integer (let () 10)))

(check-expand-core
  (let (x 10) x)
  (integer (let ((x 10)) x)))

(check-expand-core
  (let (x 10) (y 20) (+ x y))
  (integer (let ((x 10) (y 20)) (($primitive 3 +) x y))))

; boolean and

(check-expand-core (and #t) (boolean #t))
(check-expand-core (and #t #t) (boolean #t))
(check-expand-core (and #t #t #t) (boolean #t))
(check-expand-core (and #t #f #t) (boolean #f))
(check-expand-core
  (and (dynamic #f) #t #f)
  (boolean (and (dynamic #f) #t #f)))

; boolean or

(check-expand-core (or #f) (boolean #f))
(check-expand-core (or #f #f) (boolean #f))
(check-expand-core (or #f #f #f) (boolean #f))
(check-expand-core (or #f #t #f) (boolean #t))
(check-expand-core
  (or (dynamic #t) #f #t)
  (boolean (or (dynamic #t) #f #t)))

; integer +
(check-expand-core-raises (+))
(check-expand-core (+ 1) (integer 1))
(check-expand-core (+ 1 2) (integer 3))
(check-expand-core (+ 1 2 3) (integer 6))
(check-expand-core (+ (dynamic 1) 2 3) (integer (($primitive 3 +) (dynamic 1) 2 3)))
(check-expand-core (+ integer-zero integer-one) (integer 1))

; integer and
(check-expand-core (and #b111) (integer #b111))
(check-expand-core (and #b111 #b110) (integer #b110))
(check-expand-core (and #b111 #b110 #b101) (integer #b100))
(check-expand-core
  (and (dynamic #b111) #b110 #b101)
  (integer (($primitive 3 bitwise-and) (dynamic #b111) #b110 #b101)))

; integer or
(check-expand-core (or #b000) (integer #b000))
(check-expand-core (or #b000 #b001) (integer #b001))
(check-expand-core (or #b000 #b001 #b100) (integer #b101))
(check-expand-core
  (or (dynamic #b000) #b001 #b100)
  (integer (($primitive 3 bitwise-ior) (dynamic #b000) #b001 #b100)))

; integer xor
(check-expand-core (xor #b000) (integer #b000))
(check-expand-core (xor #b000 #b001) (integer #b001))
(check-expand-core (xor #b000 #b001 #b111) (integer #b110))
(check-expand-core
  (xor (dynamic #b000) #b001 #b111)
  (integer (($primitive 3 bitwise-xor) (dynamic #b000) #b001 #b111)))

; string append
(check-expand-core-raises (append))
(check-expand-core (append "a") (string "a"))
(check-expand-core (append "a" "b") (string "ab"))
(check-expand-core (append "a" "b" "c") (string "abc"))
(check-expand-core
  (append (dynamic "a") "b" "c")
  (string (($primitive 3 string-append) (dynamic "a") "b" "c")))

; string-append

(check-expand-core integer+ ((function (integer ...) integer) ($primitive 3 +)))
(check-expand-core integer- ((function (integer integer ...) integer) ($primitive 3 -)))
(check-expand-core string-append ((function (string ...) string ) ($primitive 3 string-append)))

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
