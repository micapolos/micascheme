(import (scheme) (check) (procedure))

; === once-proc ===

(check
  (equal?
    (let (($fn (once-proc (lambda () 123)))) ($fn))
    123))

(check
  (raises
    (let (($fn (once-proc (lambda () 123))))
      ($fn) ($fn))))

; === checking-once ===

(check
  (equal?
    (let (($fn (checking-once 123))) ($fn))
    123))

(check
  (raises
    (let (($fn (checking-once 123)))
    ($fn)
    ($fn))))

; === app ===

(check
  (equal?
    (app (lambda (x y) (string-append x y)) "foo" "bar")
    "foobar"))

; === values-app ===

(check
  (equal?
    (values-app
      string-append
      (0 (values))
      (1 (values "a"))
      (2 (values "b" "c")))
    "abc"))

; === partial ===

(check (equal? ((partial string-append "a" "b") "c" "d") "abcd"))

; === partial-flip ===

(check (equal? ((partial-flip string-append) "a" "b") "ab"))
(check (equal? ((partial-flip string-append "c" "d") "a" "b") "abcd"))

; === todo ===

(check (raises (todo)))
(check (raises TODO))

; === run ===

(run
  (define $box (box 10))
  (check (equal? (unbox $box) 10))
  (set-box! $box 11)
  (run) ; empty run
  (check (equal? (unbox $box) 11)))

; === run-void ===

(check (equal? (run-void) (void)))
(check (equal? (run-void (define foo "foo")) (void)))
(check (equal? (run-void (define foo "foo") "foo") (void)))

; === values-apply ===

(check (equal? (values-apply (values 1 2 3) vector) (vector 1 2 3)))

; === dot-app ===

(check (equal? (dot-app 123) 123))
(check (equal? (dot-app number->string 123) "123"))
(check (equal? (dot-app string-length number->string 123) 3))
(check (equal? (dot-app number->string string-length number->string 123) "3"))

; === dot ===

(check (equal? (app (dot) 123) 123))
(check (equal? (app (dot number->string) 123) "123"))
(check (equal? (app (dot string-length number->string) 123) 3))
(check (equal? (app (dot number->string string-length number->string) 123) "3"))

; === ignore ===

(check (equal? (ignore 10 20) 20))

; === ordered-by ===

(check
  (equal?
    (sort (ordered-by <) `(3 1 4))
    `(1 3 4)))

(check
  (equal?
    (sort
      (ordered-by < string-length)
      '("foo" "a" "foobar"))
    '("a" "foo" "foobar")))

(check
  (equal?
    (sort
      (ordered-by < string-length list->string)
      '((#\f #\o #\o) (#\a) (#\f #\o #\o #\b #\a #\r)))
    '((#\a) (#\f #\o #\o) (#\f #\o #\o #\b #\a #\r))))

; === recursive-lambda

(check
  (equal?
    (
      (recursive-lambda (fib n)
        (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
      10)
    55))

