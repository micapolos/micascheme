(import (asm lang) (asm std))

(define zero 0)
(define one 1)
(define exclamate (lambda ((s string)) (string-append s "!")))
(define (increment (i integer)) (+ i one))

(define-rules
  ((inc x) (+ x 1))
  ((dec x) (- x 1)))

(check-typed (typed string "foo") "foo")

(check-typed (let ((x 1) (y 2)) (+ x y)) 3)
(check-typed (let* ((x 1) (y (+ x 1))) (+ x y)) 3)

(check-typed zero 0)
(check-typed one 1)
(check-typed (exclamate "foo") "foo!")
(check-typed (increment 10) 11)

(check-typed (inc 10) 11)
(check-typed (dec 10) 9)

(check-typed (boolean=? #t #t) #t)
(check-typed (boolean=? #t #f) #f)

(check-typed (= 0 0) #t)
(check-typed (= 0 1) #f)

(check-typed (char=? #\a #\a) #t)
(check-typed (char=? #\a #\b) #f)

(check-typed (string=? "foo" "foo") #t)
(check-typed (string=? "foo" "bar") #f)

(check-typed (not #f) #t)
(check-typed (not #t) #f)

(check-typed (+) 0)
(check-typed (+ 1 2 3) 6)

(check-typed (- 1) -1)
(check-typed (- 10 3 2) 5)

(check-typed (>> #b11010000 2) #b00110100)
(check-typed (<< #b00110100 2) #b11010000)

(check-typed (iand #b10101010 #b11110000) #b10100000)
(check-typed (ior #b10101010 #b11110000) #b11111010)
(check-typed (ixor #b10101010 #b11110000) #b01011010)

(check-typed (string-append ) "")
(check-typed (string-append "foo" "bar" "!") "foobar!")

(check-typed (string-length "foo") 3)

(check-typed (u2 (+ 1 2)) 3)
(check-typed (u3 (+ 1 2)) 3)
(check-typed (u8 (+ 1 2)) 3)
(check-typed (u16 (+ 1 2)) 3)

(check-typed
  (bytevector (+ 1 2))
  (bytevector 3))

(check-typed
  (binary->bytevector (db-binary #x10))
  (bytevector #x10))

(check-typed
  (binary->bytevector (dw-binary #x1020))
  (bytevector #x20 #x10))

(check-typed
  (binary->bytevector
    (binary-append
      (db-binary #x10)
      (dw-binary #x2030)))
  (bytevector #x10 #x30 #x20))
