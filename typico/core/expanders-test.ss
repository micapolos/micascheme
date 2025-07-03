(import (typico base) (typico expander) (typico core expanders) (typico type) (typico core types) (typico typed))

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

; list types

(check-expand-core-type (list-of integer) (list-of-type integer-type))

; literals

(check-expand-core #f (boolean (import) #f))
(check-expand-core 10 (integer (import) 10))
(check-expand-core #\a (char (import) #\a))
(check-expand-core "foo" (string (import) "foo"))

(check-expand-core-raises 10.0)  ; integers must be exact

; function

(check-expand-core
  (=> (i integer) (s string) i)
  (
    (-> integer string integer)
    (import (scheme))
    (lambda (i s) i)))

(check-expand-core
  (=> (i integer) (s string) s)
  (
    (-> integer string string)
    (import (scheme))
    (lambda (i s) s)))

(check-expand-core
  (=> (i integer) (s string) ... i)
  (
    (-> integer string ... integer)
    (import (scheme))
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
  (integer
    (import (scheme))
    ((lambda (i s) i) 10 "foo")))

(check-expand-core
  ((=> (i integer) (s string) s) 10 "foo")
  (string
    (import (scheme))
    ((lambda (i s) s) 10 "foo")))

(check-expand-core
  ((=> (i integer) (s string) ... i) 10 "foo" "bar")
  (integer
    (import (scheme))
    ((lambda (i . s) i) 10 "foo" "bar")))

; TODO: list-of type
; (check-expand-core
;   ((=> (i integer) (s string) ... s) 10 "foo" "bar")
;   ((list-of string)
;     (import (scheme)
;     ((lambda (i . s) s) 10 "foo" "bar"))))

; if

(check-expand-core
  (if #t 10 20)
  (integer (import (scheme)) (if #t 10 20)))

(check-expand-core-raises (if #t 10 "foo"))
(check-expand-core-raises (if "foo" 10 20))
(check-expand-core-raises (if #t 10))
(check-expand-core-raises (if #t 10 20 30))

; let

(check-expand-core
  (let 10)
  (integer
    (import (scheme))
    (let () 10)))

(check-expand-core
  (let (x 10) x)
  (integer
    (import (scheme))
    (let ((x 10)) x)))

(check-expand-core
  (let (x 10) (y 20) (+ x y))
  (integer
    (import (scheme))
    (let ((x 10) (y 20)) (($primitive 3 +) x y))))

(check-expand-core-raises (let (x 10) (y (+ x 1)) (+ x y)))

; lets

(check-expand-core
  (lets 10)
  (integer (import) 10))

(check-expand-core
  (lets (x 10) x)
  (integer
    (import (scheme))
    (let ((x 10)) x)))

(check-expand-core
  (lets (x 10) (y 20) (+ x y))
  (integer
    (import (scheme))
    (let ((x 10))
      (let ((y 20))
        (($primitive 3 +) x y)))))

(check-expand-core
  (lets (x 10) (y (+ x 1)) (+ x y))
  (integer
    (import (scheme))
    (let ((x 10))
      (let ((y (($primitive 3 +) x 1)))
        (($primitive 3 +) x y)))))

; boolean and

(check-expand-core-raises (and))

(check-expand-core
  (and #t)
  (boolean (import (scheme)) (and #t)))

(check-expand-core
  (and #t #f #t)
  (boolean (import (scheme)) (and #t #f #t)))

; boolean or

(check-expand-core-raises (or))

(check-expand-core
  (or #f)
  (boolean (import (scheme)) (or #f)))

(check-expand-core
  (or #f #t #f)
  (boolean (import (scheme)) (or #f #t #f)))

; integer +
(check-expand-core-raises (+))

(check-expand-core
  (+ 1)
  (integer (import (scheme)) (($primitive 3 +) 1)))

(check-expand-core
  (+ 1 2 3)
  (integer (import (scheme)) (($primitive 3 +) 1 2 3)))

; integer and
(check-expand-core-raises (and))

(check-expand-core
  (and #b111)
  (integer (import (scheme)) (($primitive 3 bitwise-and) #b111)))

(check-expand-core
  (and #b111 #b110 #b101)
  (integer (import (scheme)) (($primitive 3 bitwise-and) #b111 #b110 #b101)))

; integer or
(check-expand-core-raises (or))

(check-expand-core
  (or #b000)
  (integer (import (scheme)) (($primitive 3 bitwise-ior) #b000)))

(check-expand-core
  (or #b111 #b110 #b101)
  (integer (import (scheme)) (($primitive 3 bitwise-ior) #b111 #b110 #b101)))

; integer xor
(check-expand-core-raises (xor))

(check-expand-core
  (xor #b000)
  (integer (import (scheme)) (($primitive 3 bitwise-xor) #b000)))

(check-expand-core
  (xor #b111 #b110 #b101)
  (integer (import (scheme)) (($primitive 3 bitwise-xor) #b111 #b110 #b101)))

; string append
(check-expand-core-raises (append))

(check-expand-core
  (append "foo")
  (string (import (scheme)) (($primitive 3 string-append) "foo")))

(check-expand-core
  (append "a" "b" "c")
  (string (import (scheme)) (($primitive 3 string-append) "a" "b" "c")))

; integer +

(check-expand-core
  integer+
  ((-> integer ... integer) (import (chezscheme)) ($primitive 3 +)))

(check-expand-core
  integer-
  ((-> integer integer ... integer) (import (chezscheme)) ($primitive 3 -)))

(check-expand-core
  string-append
  ((-> string ... string) (import (chezscheme)) ($primitive 3 string-append)))

; u8

(check-expand-core (u8 0) (u8 (import) 0))
(check-expand-core (u8 255) (u8 (import) 255))
(check-expand-core-raises (u8 -1))
(check-expand-core-raises (u8 256))

; boolean =

(check-expand-core-raises (=))
(check-expand-core-raises (= #f))
(check-expand-core-raises (= #f #t #f))
(check-expand-core-raises (= #f 10))

(check-expand-core
  (= #f #t)
  (boolean
    (import (scheme))
    (($primitive 3 boolean=?) #f #t)))

; integer =

(check-expand-core-raises (=))
(check-expand-core-raises (= 10))
(check-expand-core-raises (= 10 20 30))
(check-expand-core-raises (= 10 "a"))

(check-expand-core
  (= 10 20)
  (boolean
    (import (scheme))
    (($primitive 3 =) 10 20)))

; char =

(check-expand-core-raises (=))
(check-expand-core-raises (= #\a))
(check-expand-core-raises (= #\a #\b #\c))
(check-expand-core-raises (= #\a 10))

(check-expand-core
  (= #\a #\b)
  (boolean
    (import (scheme))
    (($primitive 3 char=?) #\a #\b)))

; string =

(check-expand-core-raises (=))
(check-expand-core-raises (= "a"))
(check-expand-core-raises (= "a" "b" "c"))
(check-expand-core-raises (= "a" 10))

(check-expand-core
  (= "a" "b")
  (boolean
    (import (scheme))
    (($primitive 3 string=?) "a" "b")))

; string-length

(check-expand-core-raises (length))
(check-expand-core-raises (length "a" "b"))
(check-expand-core-raises (length 10))

(check-expand-core
  (length "a")
  (integer
    (import (scheme))
    (($primitive 3 string-length) "a")))

; string

(check-expand-core-raises (string #\a 10))

(check-expand-core
  (string)
  (string
    (import (scheme))
    (($primitive 3 string))))

(check-expand-core
  (string #\a #\b #\c)
  (string
    (import (scheme))
    (($primitive 3 string) #\a #\b #\c)))

; ; syntax / eval

; (check-expand-core
;   (eval (syntax (+ 1 2)))
;   (integer 3))

; cond

(check-expand-core
  (cond (else 10))
  (integer (import) 10))

(check-expand-core
  (cond (#t 10) (else 20))
  (integer
    (import (scheme))
    (if #t 10 20)))

(check-expand-core
  (cond (#t 10) (#f 20) (else 30))
  (integer
    (import (scheme))
    (if #t 10 (if #f 20 30))))

(check-expand-core-raises (cond (#t 10)))  ; missing else
(check-expand-core-raises (cond (0 10) (else 20)))  ; invalid condition type
(check-expand-core-raises (cond (#t 10) (else "foo")))  ; invalid body type

; list

(check-expand-core
  (empty (list-of integer))
  ((list-of integer)
    (import)
    ()))

(check-expand-core
  (list 1 (empty (list-of integer)))
  ((list-of integer)
    (import (scheme))
    (cons 1 ())))

(check-expand-core
  (list 1 2 3)
  ((list-of integer)
    (import (scheme))
    (list 1 2 3)))

(check-expand-core
  (list 1 (list 2 3))
  ((list-of integer)
    (import (scheme))
    (cons 1 (list 2 3))))

(check-expand-core-raises (list))
(check-expand-core-raises (list 1 "foo"))
(check-expand-core-raises (list 1 (empty (list-of string))))
(check-expand-core-raises (list 1 (list "foo")))
