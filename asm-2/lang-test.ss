(import (asm-2 lang) (asm-2 std))

(define exclamate (lambda ((string s)) (string-append s "!")))
(define increment (lambda ((integer i)) (+ i 1)))

(check-asm (exclamate "foo") "foo!")
(check-asm (increment 10) 11)

(check-asm (+) 0)
(check-asm (+ 1 2 3) 6)

(check-asm (string-append ) "")
(check-asm (string-append "foo" "bar" "!") "foobar!")

(check-asm (string-length "foo") 3)
