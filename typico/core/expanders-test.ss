(import (typico base) (typico expander) (typico core expanders) (typico type) (typico core types) (typico typed))

; native

(check-expand-core
  (native integer 123)
  (integer 123))

(check-expand-core
  (native (scheme) (-> integer ... integer) +)
  ((scheme) (-> integer ... integer) +))

; primitive types

(check-expand-core-type boolean boolean-type)
(check-expand-core-type integer integer-type)
(check-expand-core-type char    char-type)
(check-expand-core-type string  string-type)
(check-expand-core-type datum   datum-type)

; function type

(check-expand-core-type
  (-> string char integer)
  (function-type (list string-type char-type) integer-type))

(check-expand-core-type
  (-> string char ... integer)
  (function-type (list* string-type char-type) integer-type))

(check-expand-core-raises (->))
(check-expand-core-raises (-> string foo integer))
(check-expand-core-raises (-> string char foo))

; generic types

(check-expand-core-type (list-of integer) (list-of-type integer-type))
(check-expand-core-type (unsafe integer) (unsafe-type integer-type))

; type classes

(check-expand-core-type unsafe generic-unsafe-type)

; literals

(check-expand-core #f (boolean #f))
(check-expand-core 10 (integer 10))
(check-expand-core #\a (char #\a))
(check-expand-core "foo" (string "foo"))

(check-expand-core-raises 10.0)  ; integers must be exact

; function

(check-expand-core
  (=> (i integer) (s string) i)
  (
    (scheme)
    (-> integer string integer)
    (lambda (i s) i)))

(check-expand-core
  (=> (i integer) (s string) s)
  (
    (scheme)
    (-> integer string string)
    (lambda (i s) s)))

(check-expand-core
  (=> (i integer) (s string) ... i)
  (
    (scheme)
    (-> integer string ... integer)
    (lambda (i . s) i)))

; TODO: list-of type
; (check-expand-core
;   (=> (i integer) (s string) ... s)
;   (
;     (-> integer string ... (list-of string))
;     (import (scheme))
;     (lambda (i . s) s)))

; application

(check-expand-core
  ((=> (i integer) (s string) i) 10 "foo")
  ((scheme) integer ((lambda (i s) i) 10 "foo")))

(check-expand-core
  ((=> (i integer) (s string) s) 10 "foo")
  ((scheme) string ((lambda (i s) s) 10 "foo")))

(check-expand-core
  ((=> (i integer) (s string) ... i) 10 "foo" "bar")
  ((scheme) integer ((lambda (i . s) i) 10 "foo" "bar")))

(check-expand-core
  ((=> (i integer) (s string) ... s) 10 "foo" "bar")
  ((scheme) (list-of string) ((lambda (i . s) s) 10 "foo" "bar")))

; if

(check-expand-core
  (if #t 10 20)
  ((scheme) integer (if #t 10 20)))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

; let

(check-expand-core
  (let 10)
  ((scheme) integer (let () 10)))

(check-expand-core
  (let (x 10) x)
  ((scheme) integer (let ((x 10)) x)))

(check-expand-core
  (let (x 10) (y 20) (+ x y))
  ((scheme) integer (let ((x 10) (y 20)) (+ x y))))

(check-expand-core-raises (let (x 10) (y (+ x 1)) (+ x y)))

; lets

(check-expand-core
  (lets 10)
  (integer 10))

(check-expand-core
  (lets (x 10) x)
  ((scheme) integer (let ((x 10)) x)))

(check-expand-core
  (lets (x 10) (y 20) (+ x y))
  ((scheme) integer
    (let ((x 10))
      (let ((y 20))
        (+ x y)))))

(check-expand-core
  (lets (x 10) (y (+ x 1)) (+ x y))
  ((scheme) integer
    (let ((x 10))
      (let ((y (+ x 1)))
        (+ x y)))))

; boolean and

(check-expand-core-raises (and))

(check-expand-core
  (and #t)
  ((scheme) boolean (and #t)))

(check-expand-core
  (and #t #f #t)
  ((scheme) boolean (and #t #f #t)))

; boolean or

(check-expand-core-raises (or))

(check-expand-core
  (or #f)
  ((scheme) boolean (or #f)))

(check-expand-core
  (or #f #t #f)
  ((scheme) boolean (or #f #t #f)))

; integer +
(check-expand-core-raises (+))

(check-expand-core
  (+ 1)
  ((scheme) integer (+ 1)))

(check-expand-core
  (+ 1 2 3)
  ((scheme) integer (+ 1 2 3)))

; integer and
(check-expand-core-raises (and))

(check-expand-core
  (and #b111)
  ((scheme) integer (bitwise-and #b111)))

(check-expand-core
  (and #b111 #b110 #b101)
  ((scheme) integer (bitwise-and #b111 #b110 #b101)))

; integer or
(check-expand-core-raises (or))

(check-expand-core
  (or #b000)
  ((scheme) integer (bitwise-ior #b000)))

(check-expand-core
  (or #b111 #b110 #b101)
  ((scheme) integer (bitwise-ior #b111 #b110 #b101)))

; integer xor
(check-expand-core-raises (xor))

(check-expand-core
  (xor #b000)
  ((scheme) integer (bitwise-xor #b000)))

(check-expand-core
  (xor #b111 #b110 #b101)
  ((scheme) integer (bitwise-xor #b111 #b110 #b101)))

; string append
(check-expand-core-raises (append))

(check-expand-core
  (append "foo")
  ((scheme) string (string-append "foo")))

(check-expand-core
  (append "a" "b" "c")
  ((scheme) string (string-append "a" "b" "c")))

; integer +

(check-expand-core
  integer+
  ((scheme) (-> integer ... integer) +))

(check-expand-core
  integer-
  ((scheme) (-> integer integer ... integer) -))

(check-expand-core
  string-append
  ((scheme) (-> string ... string) string-append))

; u8

(check-expand-core (u8 0) (u8 0))
(check-expand-core (u8 255) (u8 255))
(check-expand-core-raises (u8 -1))
(check-expand-core-raises (u8 256))

; boolean =

(check-expand-core-raises (=))
(check-expand-core-raises (= #f))
(check-expand-core-raises (= #f #t #f))
(check-expand-core-raises (= #f 10))

(check-expand-core
  (= #f #t)
  ((scheme) boolean (boolean=? #f #t)))

; integer =

(check-expand-core-raises (=))
(check-expand-core-raises (= 10))
(check-expand-core-raises (= 10 20 30))
(check-expand-core-raises (= 10 "a"))

(check-expand-core
  (= 10 20)
  ((scheme) boolean (= 10 20)))

; char =

(check-expand-core-raises (=))
(check-expand-core-raises (= #\a))
(check-expand-core-raises (= #\a #\b #\c))
(check-expand-core-raises (= #\a 10))

(check-expand-core
  (= #\a #\b)
  ((scheme) boolean (char=? #\a #\b)))

; string =

(check-expand-core-raises (=))
(check-expand-core-raises (= "a"))
(check-expand-core-raises (= "a" "b" "c"))
(check-expand-core-raises (= "a" 10))

(check-expand-core
  (= "a" "b")
  ((scheme) boolean (string=? "a" "b")))

; string-length

(check-expand-core-raises (length))
(check-expand-core-raises (length "a" "b"))
(check-expand-core-raises (length 10))

(check-expand-core
  (length "a")
  ((scheme) integer (string-length "a")))

; string

(check-expand-core-raises (string #\a 10))

(check-expand-core
  (string)
  ((scheme) string (string)))

(check-expand-core
  (string #\a #\b #\c)
  ((scheme) string (string #\a #\b #\c)))

; ; syntax / eval

; (check-expand-core
;   (eval (syntax (+ 1 2)))
;   (integer 3))

; cond

(check-expand-core
  (cond (else 10))
  (integer 10))

(check-expand-core
  (cond (#t 10) (else 20))
  ((scheme) integer (if #t 10 20)))

(check-expand-core
  (cond (#t 10) (#f 20) (else 30))
  ((scheme) integer (if #t 10 (if #f 20 30))))

(check-expand-core-raises (cond (#t 10)))  ; missing else
(check-expand-core-raises (cond (0 10) (else 20)))  ; invalid condition type
(check-expand-core-raises (cond (#t 10) (else "foo")))  ; invalid body type

; list

(check-expand-core
  (empty (list-of integer))
  ((list-of integer) '()))

(check-expand-core
  (list 1 (empty (list-of integer)))
  ((scheme) (list-of integer) (cons 1 '())))

(check-expand-core
  (list 1 2 3)
  ((scheme) (list-of integer) (list 1 2 3)))

(check-expand-core
  (list 1 (list 2 3))
  ((scheme) (list-of integer) (cons 1 (list 2 3))))

(check-expand-core-raises (list))
(check-expand-core-raises (list 1 "foo"))
(check-expand-core-raises (list 1 (empty (list-of string))))
(check-expand-core-raises (list 1 (list "foo")))

; list length

(check-expand-core
  (length (empty (list-of integer)))
  ((scheme) integer (length '())))

(check-expand-core
  (length (list 1 2 3))
  ((scheme) integer (length (list 1 2 3))))

; pure

(check-expand-core
  (pure optional 123)
  ((optional integer) 123))

(check-expand-core
  (pure unsafe 123)
  ((unsafe integer) 123))

(check-expand-core
  (pure list-of 123)
  ((scheme) (list-of integer) (list 123)))

(check-expand-core-raises (pure integer 123))
