(import (asm-2 lang) (asm-2 std))

(define zero 0)
(define one 1)
(define exclamate (lambda ((string s)) (string-append s "!")))
(define (increment (integer i)) (+ i one))

(check-asm zero 0)
(check-asm one 1)
(check-asm (exclamate "foo") "foo!")
(check-asm (increment 10) 11)

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
