(import
  (check)
  (tipsy)
  (tipsy-expr)
  (micascheme))

; type macros
(check (equal? Boolean (boolean-type)))
(check (equal? Number (number-type)))
(check (equal? String (string-type)))
(check
  (equal?
    (Lambda (String Number) Boolean)
    (lambda-type (list String Number) Boolean)))
(check (equal? Type (type-type)))
(check (equal? (List String) (list-type String)))

; of
(check (equal? (tipsy (of + String)) +))
(check (equal? (tipsy (typeof (of + String))) String))

; boolean literal
(check (equal? (tipsy #t) #t))
(check (equal? (tipsy #f) #f))
(check (equal? (tipsy (typeof #t)) Boolean))
(check (equal? (tipsy (typeof #f)) Boolean))

; number literal
(check (equal? (tipsy 1) 1))
(check (equal? (tipsy (typeof 1)) Number))

; string literal
(check (equal? (tipsy "foo") "foo"))
(check (equal? (tipsy (typeof "foo")) String))

; if
(check (equal? (tipsy (if #t "true" "false")) "true"))
(check (equal? (tipsy (if #f "true" "false")) "false"))

; lambda
(check (procedure? (tipsy (lambda () "foo"))))
(check (equal? ((tipsy (lambda () "foo"))) "foo"))
(check (equal? (tipsy (typeof (lambda () "foo"))) (Lambda () String)))

; lambda
(check (procedure? (tipsy (lambda ((String a) (String b)) a))))
(check
  (equal?
    ((tipsy (lambda ((String a) (String b)) a)) "foo" "bar")
    "foo"))
(check
  (equal?
    (tipsy (typeof (lambda ((String a) (String b)) a)))
    (Lambda (String String) String)))

; define-type
(define-type string-append
  (Lambda (String String) String))

(check (equal? (tipsy string-append) string-append))
(check
  (equal?
    (tipsy (typeof string-append))
    (Lambda (String String) String)))

(check (equal? (tipsy (string-append "foo" "bar")) "foobar"))

(define-tipsy foo 123)
