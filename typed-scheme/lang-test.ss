(import
  (micascheme)
  (typed-scheme lang)
  (typed-scheme type)
  (typed-scheme types))

(define-type a-null null-type-definition)
(define-type a-boolean boolean-type-definition)
(define-type a-string string-type-definition)
(define-type a-number number-type-definition)
(define-type a-pair pair-type-definition)

(check (equal? (typed #t) #t))
(check (equal? (typed #f) #f))
(check (equal? (typed 123) 123))
(check (equal? (typed "foo") "foo"))

(assume-type string-append (a-lambda (a-string a-string) a-string))
(check (equal? (typed string-append) string-append))

(assume-type (string=? a-string a-string) a-boolean)
(check (equal? (typed string=?) string=?))

(check (equal? (typed (string=? "foo" "bar")) #f))
(check (equal? (typed (string-append "foo" "bar")) "foobar"))

(define-typed foo "foo")
(define-typed bar "bar")

(check (equal? (typed foo) "foo"))
(check (equal? (typed bar) "bar"))
(check (equal? (typed (string-append foo bar)) "foobar"))

(define-typed string+
  (lambda ((a-string a) (a-string b))
    (string-append a b)))

(check (equal? (typed (string+ foo bar)) "foobar"))

(define-typed
  (string++ (a-string a) (a-string b))
    (string-append a b))

(check (equal? (typed (string++ foo bar)) "foobar"))

(check (equal? (typed (expect a-string "foo")) "foo"))

(define-typed string-len (assume (a-lambda (a-string) a-number) string-length))

(check (equal? (typed (string-len "foo")) 3))
