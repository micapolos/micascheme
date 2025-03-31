(import
  (only (micascheme) check equal? string-append string=? string-length null = + - number->string zero?)
  (typed-scheme lang)
  (typed-scheme type)
  (typed-scheme types))

(define-type any-null null-type-definition)
(define-type any-boolean boolean-type-definition)
(define-type any-string string-type-definition)
(define-type any-char char-type-definition)
(define-type any-number number-type-definition)
(define-type any-pair pair-type-definition)

(check (equal? (typed null) null))
(check (equal? (typed #t) #t))
(check (equal? (typed #f) #f))
(check (equal? (typed #\a) #\a))
(check (equal? (typed "foo") "foo"))
(check (equal? (typed 123) 123))

(assume-type string-append (any-lambda (any-string any-string) any-string))
(check (equal? (typed string-append) string-append))

(assume-type (string=? any-string any-string) any-boolean)
(check (equal? (typed string=?) string=?))

(check (equal? (typed (string=? "foo" "bar")) #f))
(check (equal? (typed (string-append "foo" "bar")) "foobar"))

(define foo "foo")
(define bar "bar")

(check (equal? (typed foo) "foo"))
(check (equal? (typed bar) "bar"))
(check (equal? (typed (string-append foo bar)) "foobar"))

(define string+
  (lambda ((any-string a) (any-string b))
    (string-append a b)))

(check (equal? (typed (string+ foo bar)) "foobar"))

(define
  (string++ (any-string a) (any-string b))
    (string-append a b))

(check (equal? (typed (string++ foo bar)) "foobar"))

(check (equal? (typed (expect any-string "foo")) "foo"))

(define string-len (assume (any-lambda (any-string) any-number) string-length))

(check (equal? (typed (string-len "foo")) 3))

(assume-type = (any-lambda (any-number any-number) any-boolean))
(assume-type + (any-lambda (any-number any-number) any-number))
(assume-type - (any-lambda (any-number any-number) any-number))
(assume-type number->string (any-lambda (any-number) any-string))
(assume-type zero? (any-lambda (any-number) any-boolean))

; recursive declaration

(define (string+numbers (any-string $string) (any-number $number))
  (expect any-string
    (if (zero? $number)
      $string
      (string+numbers
        (string-append $string (number->string $number))
        (- $number 1)))))

(check (equal? (typed (string+numbers "" 5)) "54321"))

(assume-type equal? (forall (x) (any-lambda (x x) any-boolean)))
