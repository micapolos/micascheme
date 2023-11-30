(import (check) (procedure))

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
      (2 (values string-append "a"))
      (0 (values))
      (1 (values "b"))
      (2 (values "c" "d")))
    "abcd"))

; === partial ===

(check (equal? ((partial string-append "a" "b") "c" "d") "abcd"))

; === todo ===

(check (raises? (lambda () (todo))))
(check (raises? (lambda () TODO)))

; === run ===

(lets
  ($box (box 10))
  (run
    (check (equal? (unbox $box) 10))
    (set-box! $box 11)
    (check (equal? (unbox $box) 11))))
