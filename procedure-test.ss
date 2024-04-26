(import (scheme) (check) (procedure))

; === once-proc ===

(check
  (equal?
    (let (($fn (once-proc (lambda () 123)))) ($fn))
    123))

(check
  (raises?
    (lambda ()
      (let (($fn (once-proc (lambda () 123))))
        ($fn) ($fn)))))

; === checking-once ===

(check
  (equal?
    (let (($fn (checking-once 123))) ($fn))
    123))

(check
  (raises?
    (lambda ()
      (let (($fn (checking-once 123)))
      ($fn)
      ($fn)))))

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

; === todo ===

(check (raises? (lambda () (todo))))
(check (raises? (lambda () TODO)))

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
