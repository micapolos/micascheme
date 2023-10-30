(import (micascheme) (tico-3))

(define env (environment `(micascheme)))

(check
  (equal?
    (scope-value (scope env (stack)) `+)
    (constant +)))

(check
  (equal?
    (scope-value (scope env (stack)) `cond)
    (transformer (top-level-syntax `cond env))))

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

(let
  (($transformer (lambda ($syntax) $syntax)))
  (check
    (equal?
      (scope-value
        (scope env
          (stack
            (cons `cond (transformer $transformer))))
        `cond)
      (transformer $transformer))))

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

; --- literal

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
    (syntax->thunk #`#\space)
    (thunk (constant #\space) #\space)))

(check
  (equal?
    (syntax->thunk #`"foo")
    (thunk (constant "foo") "foo")))

; --- native

(check
  (equal?
    (syntax->thunk
      #`(native (string-append "foo" "bar")))
    (thunk
      (constant "foobar")
      `(string-append "foo" "bar"))))

(check
  (raises?
    (lambda ()
      (syntax->thunk
        #`(native unbound)))))

; --- identifier

(check
  (equal?
    (syntax->thunk #`string-append)
    (thunk
      (constant string-append)
      `string-append)))

; --- application

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

; --- rec lambda

(let
  (($thunk
    (syntax->thunk
      #`(lambda fib (n)
          (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))))
  (check
    (equal?
      (thunk-datum $thunk)
      `(rec fib
        (lambda (n)
          (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))))
  (check
    (equal?
      ((constant-value (thunk-value $thunk)) 10)
      55)))

(check
  (equal?
    (syntax->thunk
      #`(
        (lambda fib (n)
          (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1)))))
        10))
    (thunk
      (constant 55)
      `(
        (rec fib
          (lambda (n)
            (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1))))))
        10))))

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
  (equal?
    (syntax->thunk
      #`(compile-time
        (
          (lambda fib (n)
            (if (< n 2) n (+ (fib (- n 2)) (fib (- n 1)))))
          10)))
    (thunk (constant 55) 55)))

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

; --- transformers

(check
  (equal?
    (thunk-datum
      (syntax->thunk
        #`(lambda ($x)
          (cond
            ($x "foo")
            (else "bar")))))
    `(lambda ($x)
      (if $x "foo" "bar"))))
