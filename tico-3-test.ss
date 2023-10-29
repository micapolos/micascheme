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

; --- compiler

(check
  (equal?
    (app
      (compiler `(+ a b))
      (stack (cons `a "a") (cons `b "b")))
    (compiled
      (stack (cons `a "a") (cons `b "b"))
      `(+ a b))))

(check
  (equal?
    (app
      (compiler+binding
        (compiler `(+ a b))
        (cons `c "c"))
      (stack (cons `a "a") (cons `b "b")))
    (compiled
      (stack (cons `a "a") (cons `b "b") (cons `c "c"))
      `(+ a b))))

(check
  (equal?
    (compiler-compiled
      (compilers-flatten
        (list
          (compiler+binding
            (compiler 10)
            (cons `a "a"))
          (compiler+binding
            (compiler 20)
            (cons `b "b")))))
    (compiled
      (stack (cons `a "a") (cons `b "b"))
      `(10 20))))

; --- thunk-compiler->symbolize

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (thunk-compiler->symbolize
          (compiler (thunk (constant "foo") 'constant-string)))))
    (compiled
      (stack)
      (thunk (constant "foo") 'constant-string))))

(check
  (equal?
    (compiler-compiled
      (thunk-compiler->symbolize
        (compiler (thunk (constant "foo") "foo"))))
    (compiled
      (stack)
      (thunk (constant "foo") "foo"))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (thunk-compiler->symbolize
          (compiler (thunk (constant "foo") `(dynamic-string))))))
    (compiled
      (stack (cons `$tmp-0 (thunk "foo" `(dynamic-string))))
      (thunk (constant "foo") `$tmp-0))))

(check
  (equal?
    (compiler-compiled
      (thunk-compiler->symbolize
        (compiler (thunk (variable 3) `(some-string)))))
    (compiled
      (stack)
      (thunk (variable 3) `(some-string)))))

; --- datum->thunk

(check
  (equal?
    (syntax->thunk #`#f)
    (thunk #f #f)))

(check
  (equal?
    (syntax->thunk #`123)
    (thunk 123 123)))

(check
  (equal?
    (syntax->thunk #`"foo")
    (thunk "foo" "foo")))

(check
  (equal?
    (syntax->thunk #`string-append)
    (thunk string-append `string-append)))

(check
  (equal?
    (syntax->thunk #`(string-append "foo" "bar"))
    (thunk
      "foobar"
      `(string-append "foo" "bar"))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (syntax->thunk
        #`(string-append
          (string-append "fo" "o")
          (string-append "ba" "r"))))
    (thunk
      "foobar"
      `(lets
        ($tmp-0 (string-append "fo" "o"))
        ($tmp-1 (string-append "ba" "r"))
        (string-append $tmp-0 $tmp-1)))))

(let (($thunk (syntax->thunk #`(lambda ($x $y) "foo"))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) "foo")))
  (check (equal? (app (thunk-value $thunk) 1 2) "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) $x))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) $x)))
  (check (equal? (app (thunk-value $thunk) "foo" "bar") "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) (string-append $x $y)))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) (string-append $x $y))))
  (check (equal? (app (thunk-value $thunk) "foo" "bar") "foobar")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) (string-append "foo" "bar")))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) (string-append "foo" "bar"))))
  (check (equal? (app (thunk-value $thunk) "goo" "gar") "foobar")))

; --- assert

(check
  (equal?
    (syntax->thunk #`(assert (= 1 1) "foo"))
    (thunk "foo" "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x) (assert (= 1 1) (string-append $x "!"))))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x) (string-append $x "!"))))
  (check (equal? (app (thunk-value $thunk) "foo") "foo!")))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(assert (= 1 2) "foo")))))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(lambda (x) (assert (= 1 2) "foo"))))))

(check
  (raises?
    (lambda ()
      (syntax->thunk #`(lambda (x) (assert (= x 1) "foo"))))))
