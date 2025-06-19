(import
  (rename (micascheme)
    (+ %+)
    (string-append %string-append)
    (string-length %string-length))
  (asm-2 lang)
  (asm-2 typed))

(define-typed + (typed (procedure integer integer) %+))
(define-typed string-append (typed (procedure string string) %string-append))
(define-typed string-length (typed (procedure (string) integer) %string-length))

(check (equal? (asm (+)) 0))
(check (equal? (asm (+ 1 2 3)) 6))

(check (equal? (asm (string-append )) ""))
(check (equal? (asm (string-append "foo" "bar" "!")) "foobar!"))

(check (equal? (asm (string-length "foo")) 3))
