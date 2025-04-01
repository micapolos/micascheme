(import
  (only (micascheme) check equal? string-append string=? string-length null = + - number->string zero? immutable-vector)
  (typed-scheme typed)
  (typed-scheme lang)
  (typed-scheme type)
  (typed-scheme types))

(define-type any-null null-type-definition)
(define-type any-boolean boolean-type-definition)
(define-type any-string string-type-definition)
(define-type any-char char-type-definition)
(define-type any-number number-type-definition)
(define-type any-pair pair-type-definition)

(check (equal? (expr null) (typed null-type null)))
(check (equal? (expr #t) (typed boolean-type #t)))
(check (equal? (expr #f) (typed boolean-type #f)))
(check (equal? (expr #\a) (typed char-type #\a)))
(check (equal? (expr "foo") (typed string-type "foo")))
(check (equal? (expr 123) (typed number-type 123)))

(assume-type string-append (any-lambda (any-string any-string) any-string))
(check
  (equal?
    (expr string-append)
    (typed
      (lambda-type (immutable-vector string-type string-type) string-type)
      string-append)))

(assume-type (string=? any-string any-string) any-boolean)
(check
  (equal?
    (expr string=?)
    (typed
      (lambda-type (immutable-vector string-type string-type) boolean-type)
      string=?)))

(check (equal? (expr (string=? "foo" "bar")) (typed boolean-type #f)))
(check (equal? (expr (string-append "foo" "bar")) (typed string-type "foobar")))

(define foo "foo")
(define bar "bar")

(check (equal? (expr foo) (typed string-type "foo")))
(check (equal? (expr bar) (typed string-type "bar")))
(check (equal? (expr (string-append foo bar)) (typed string-type "foobar")))

(define string+
  (lambda ((any-string a) (any-string b))
    (string-append a b)))

(check (equal? (expr (string+ foo bar)) (typed string-type "foobar")))

(define
  (string++ (any-string a) (any-string b))
    (string-append a b))

(check (equal? (expr (string++ foo bar)) (typed string-type "foobar")))

(check (equal? (expr (expect any-string "foo")) (typed string-type "foo")))

(define string-len (assume (any-lambda (any-string) any-number) string-length))

(check (equal? (expr (string-len "foo")) (typed number-type 3)))

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

(check (equal? (expr (string+numbers "" 5)) (typed string-type "54321")))

(assume-type equal? (forall (x) (any-lambda (x x) any-boolean)))
