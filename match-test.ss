(import (scheme) (check) (match) (syntax))

; === match?

(check (equal? (match? #t #t "true") "true"))
(check (equal? (match? #f #t "true") #f))

(check (equal? (match? #\a #\a "a") "a"))
(check (equal? (match? #\b #\a "a") #f))

(check (equal? (match? 128 128 "128") "128"))
(check (equal? (match? 129 128 "128") #f))

(check (equal? (match? "foo" "foo" "foo!") "foo!"))
(check (equal? (match? "bar" "foo" "foo!") #f))

(check (equal? (match? "foo" foo (string-append foo "!")) "foo!"))

(define-property string match-prim?
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ val (_ s) body)
        #`(and
          (string? val)
          (let ((s val))
            body))))))

(define-keyword number)

(define-property number match-prim?
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ val (_ n) body)
        #`(and
          (number? val)
          (let ((n val))
            body))))))

(check (equal? (match? "foo" (string $string) (string-append $string "!")) "foo!"))
(check (equal? (match? 123 (string $string) (string-append $string "!")) #f))

(check (equal? (match? "foo" (number $number) (+ $number 1)) #f))
(check (equal? (match? 123 (number $number) (+  $number 1)) 124))

(define-property cons match-prim?
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ val (_ a b) body)
        #`(and
          (pair? val)
          (let ((a (car val)) (b (cdr val)))
            body))))))

(check
  (equal?
    (match? "foo" (cons a b) (cons b a))
    #f))

(check
  (equal?
    (match? (cons 1 2) (cons a b) (cons b a))
    (cons 2 1)))

(check
  (equal?
    (match? (list 1 2) (cons a (cons b c)) (cons b (cons a c)))
    (list 2 1)))

; === match

(check (equal? (match "foo" (string $string) (string-append $string "!")) "foo!"))
(check (raises (match 123 (string $string) (string-append $string "!"))))

; === match-case?

(check
  (equal?
    (match-case? "foo"
      ((string s) (string-append s "!"))
      ((number n) (number->string n)))
    "foo!"))

(check
  (equal?
    (match-case? 123
      ((string s) (string-append s "!"))
      ((number n) (+ n 1)))
    124))

(check
  (equal?
    (match-case? #\a
      ((string s) (string-append s "!"))
      ((number n) (+ n 1)))
    #f))

(check
  (equal?
    (match-case? #\a
      ((string s) (string-append s "!"))
      ((number n) (+ n 1))
      (x (format "char ~a" x)))
    "char a"))

; === match-case

(check
  (equal?
    (match-case "foo"
      ((string s) (string-append s "!"))
      ((number n) (number->string n)))
    "foo!"))

(check
  (raises
    (match-case #\a
      ((string s) (string-append s "!"))
      ((number n) (+ n 1)))))
