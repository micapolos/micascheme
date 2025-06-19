(import
  (rename (micascheme)
    (+ %+)
    (string-append %string-append)
    (string-length %string-length)
    (define %define))
  (asm-2 lang))

(define + (typed (procedure integer integer) %+))
(define string-append (typed (procedure string string) %string-append))
(define string-length (typed (procedure (string) integer) %string-length))

(define exclamate (lambda ((string s)) (string-append s "!")))
(define increment (lambda ((integer i)) (+ i 1)))

(check (equal? (asm (exclamate "foo")) "foo!"))
(check (equal? (asm (increment 10)) 11))

(check (equal? (asm (+)) 0))
(check (equal? (asm (+ 1 2 3)) 6))

(check (equal? (asm (string-append )) ""))
(check (equal? (asm (string-append "foo" "bar" "!")) "foobar!"))

(check (equal? (asm (string-length "foo")) 3))
