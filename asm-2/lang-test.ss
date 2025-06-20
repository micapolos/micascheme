(import (asm-2 lang) (asm-2 std))

(define zero 0)
(define one 1)
(define exclamate (lambda ((string s)) (string-append s "!")))
(define (increment (integer i)) (+ i one))

(check-asm (typed string "foo") "foo")

(check-asm zero 0)
(check-asm one 1)
(check-asm (exclamate "foo") "foo!")
(check-asm (increment 10) 11)

(check-asm (boolean=? #t #t) #t)
(check-asm (boolean=? #t #f) #f)

(check-asm (= 0 0) #t)
(check-asm (= 0 1) #f)

(check-asm (char=? #\a #\a) #t)
(check-asm (char=? #\a #\b) #f)

(check-asm (string=? "foo" "foo") #t)
(check-asm (string=? "foo" "bar") #f)

(check-asm (not #f) #t)
(check-asm (not #t) #f)

(check-asm (+) 0)
(check-asm (+ 1 2 3) 6)

(check-asm (- 1) -1)
(check-asm (- 10 3 2) 5)

(check-asm (>> #b11010000 2) #b00110100)
(check-asm (<< #b00110100 2) #b11010000)

(check-asm (and #b10101010 #b11110000) #b10100000)
(check-asm (or #b10101010 #b11110000) #b11111010)
(check-asm (xor #b10101010 #b11110000) #b01011010)

(check-asm (string-append ) "")
(check-asm (string-append "foo" "bar" "!") "foobar!")

(check-asm (string-length "foo") 3)

(check-asm
  (bytevector (+ 1 2))
  (bytevector 3))

(check-asm
  (binary->bytevector (db-binary #x10))
  (bytevector #x10))

(check-asm
  (binary->bytevector (dw-binary #x1020))
  (bytevector #x20 #x10))

(check-asm
  (binary->bytevector (binary-append (db-binary #x10) (db-binary #x20)))
  (bytevector #x10 #x20))

(check-asm
  (binary->bytevector
    (asm-binary
      (org 100)
      (db 10)))
  (bytevector 10))

; (check-asm
;   (binary->bytevector
;     (asm-binary
;       (org 100)
;       (db end)
;       (label end)))
;   (bytevector 101))

; (check-asm
;   (binary->bytevector (asm-binary
;     (org 100)
;     (label begin)
;     (db (- end begin))
;     (label end)))
;   (bytevector 1 5))
