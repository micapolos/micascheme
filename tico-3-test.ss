(import (micascheme) (tico-3))

(define env (environment `(micascheme)))

(check
  (equal?
    (scope-value (scope env (stack)) `+)
    (constant +)))

(check
  (raises?
    (lambda ()
      (scope-value
        (scope env (stack)) `foo))))

(check
  (equal?
    (scope-value
      (scope env
        (stack
          (cons `+ (constant "foo"))))
      `+)
    (constant "foo")))

(check
  (equal?
    (scope-value
      (scope env
        (stack
          (cons `v0 (hole))
          (cons `+ (hole))
          (cons `v1 (hole))
          (cons `v2 (hole))))
      `+)
    (variable 2)))

; --- datum->thunk

(check
  (equal?
    (syntax->thunk #`#f)
    (thunk (constant #f) #f)))

(check
  (equal?
    (syntax->thunk #`123)
    (thunk (constant 123) 123)))

(check
  (equal?
    (syntax->thunk #`"foo")
    (thunk (constant "foo") "foo")))

(check
  (equal?
    (syntax->thunk #`string-append)
    (thunk
      (constant string-append)
      `string-append)))

(check
  (equal?
    (syntax->thunk #`(string-append "foo" "bar"))
    (thunk
      (constant "foobar")
      `(string-append "foo" "bar"))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (syntax->thunk
        #`(string-append
          (string-append "fo" "o")
          (string-append "ba" "r"))))
    (thunk
      (constant "foobar")
      `(string-append
          (string-append "fo" "o")
          (string-append "ba" "r")))))


; --- if

(check
  (equal?
    (syntax->thunk #`(if (= 1 1) "ok" (throw not-ok)))
    (thunk (constant "ok") "ok")))

(check
  (equal?
    (syntax->thunk #`(if (= 1 2) (throw ok) "not-ok"))
    (thunk (constant "not-ok") "not-ok")))

(check
  (equal?
    (syntax->thunk
      #`((lambda ($x) (string-append $x (if (= 1 1) "!" "?"))) "foo"))
    (thunk
      (constant "foo!")
      `((lambda ($x) (string-append $x "!")) "foo"))))

(check
  (equal?
    (syntax->thunk
      #`((lambda ($x) (string-append $x (if (= 1 2) "!" "?"))) "foo"))
    (thunk
      (constant "foo?")
      `((lambda ($x) (string-append $x "?")) "foo"))))

(check
  (equal?
    (syntax->thunk #`((lambda ($x) (if $x "ok" "not-ok")) (= 1 1)))
    (thunk
      (constant "ok")
      `((lambda ($x) (if $x "ok" "not-ok")) (= 1 1)))))

(check
  (equal?
    (syntax->thunk #`((lambda ($x) (if $x "ok" "not-ok")) (= 1 2)))
    (thunk
      (constant "not-ok")
      `((lambda ($x) (if $x "ok" "not-ok")) (= 1 2)))))

; --- lambda

(let (($thunk (syntax->thunk #`(lambda ($x $y) "foo"))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) "foo")))
  (check (equal? (app (constant-value (thunk-value $thunk)) 1 2) "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) $x))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) $x)))
  (check (equal? (app (constant-value (thunk-value $thunk)) "foo" "bar") "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) (string-append $x $y)))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) (string-append $x $y))))
  (check (equal? (app (constant-value (thunk-value $thunk)) "foo" "bar") "foobar")))

(let
  (($thunk
    (with-generate-temporary-seed $tmp
      (syntax->thunk
        #`(lambda ($x $y)
          (string-append $x $y (string-append "!" "?")))))))
  (check
    (equal?
      (thunk-datum $thunk)
      `(lambda ($x $y)
        (string-append $x $y (string-append "!" "?")))))
  (check
    (equal?
      (app (constant-value (thunk-value $thunk)) "foo" "bar")
      "foobar!?")))

; --- compile-time

(check
  (equal?
    (syntax->thunk #`(compile-time (string-append "foo" "bar")))
    (thunk (constant "foobar") "foobar")))

(check
  (equal?
    (syntax->thunk
      #`(
        (lambda ($x $y)
          (compile-time (string-append "foo" "bar")))
        "goo" "gar"))
    (thunk
      (constant "foobar")
      `(
        (lambda ($x $y) "foobar")
        "goo" "gar"))))

(check
  (raises?
    (lambda ()
      (syntax->thunk
        #`(
          (lambda ($x $y)
            (compile-time (string-append $x $y)))
          "goo" "gar")))))

; --- testing

(check
  (equal?
    (syntax->thunk #`(testing (= 1 1) "foo"))
    (thunk (constant "foo") "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x) (testing (= 1 1) (string-append $x "!"))))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x) (string-append $x "!"))))
  (check (equal? (app (constant-value (thunk-value $thunk)) "foo") "foo!")))

(check
  (equal?
    (syntax->thunk
      #`(let (($x "foo") ($y "foo"))
        (testing
          (equal? $x $y)
          (string-append $x $y))))
    (thunk
      (constant "foofoo")
      `(let (($x "foo") ($y "foo"))
        (string-append $x $y)))))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(testing (= 1 2) "foo")))))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(lambda (x) (testing (= 1 2) "foo"))))))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(lambda (x) (testing (= x 1) "foo"))))))
